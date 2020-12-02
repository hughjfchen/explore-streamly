{- | This server accepts a stream of command words separated by space characters
   and responds with outputs of the commands.
 -}

{-# LANGUAGE TupleSections #-}
{-# LANGUAGE FlexibleContexts #-}

module Main (main) where

import System.Environment (getArgs)
import Control.Monad (when)
import Data.Word (Word8)
import Data.Function ((&))
import Network.Socket (Socket, PortNumber)
import System.Random (randomIO)

import qualified Data.Map.Strict as Map
import Control.Monad.State (MonadState, get, modify, runStateT)
import Control.Monad.IO.Class (MonadIO(liftIO))
import Control.Monad (void)

-- Streamly core
import Streamly
import qualified Streamly.Prelude as S

-- | Currently, all streamly APIs, public or internal modules,  should be imported qualifiedly

-- | Streamly Public APIs
-- Streamly data and types
import qualified Streamly.Data.Fold as F
import qualified Streamly.Data.Unfold as UF
import qualified Streamly.Data.Unicode.Stream as U
import qualified Streamly.Memory.Array as A

-- Streamly network
import qualified Streamly.Network.Socket as NS
import qualified Streamly.Network.Inet.TCP as TCP

-- Streamly filesystem
import qualified Streamly.FileSystem.Handle as FS


-- | Streamly internal APIs
import qualified Streamly.Internal.Data.Fold as IF
import qualified Streamly.Internal.Data.Unfold as IUF
import qualified Streamly.Internal.Data.Unicode.Stream as IU
import qualified Streamly.Internal.Memory.Array as IA
import qualified Streamly.Internal.Network.Socket as INS
import qualified Streamly.Internal.Network.Inet.TCP as ITCP
import qualified Streamly.Internal.FileSystem.Handle as IFS

import qualified Streamly.Internal.Control.Monad as IM
import qualified Streamly.Internal.Data.Time.Clock as IClock

-- Utilities
sendValue :: Show a => Socket -> a -> IO ()
sendValue sk x =
  S.fromList (show x ++ "\n")
  & U.encodeLatin1
  & S.fold (A.writeN 60)
  >>= INS.writeChunk sk

-- command handlers
time :: Socket -> IO ()
time sk = IClock.getTime IClock.Monotonic >>= sendValue sk

random :: Socket -> IO ()
random sk = (randomIO :: IO Int) >>= sendValue sk

def :: (String, Socket) -> IO ()
def (str, sk) = return ("Unknown command:" ++ str) >>= sendValue sk

commands :: Map.Map String (F.Fold IO Socket ())
commands = Map.fromList
  [ ("time", F.drainBy time)
  , ("random", F.drainBy random)
  ]

demux :: F.Fold IO (String, Socket) ()
demux = IF.demuxDefault_ commands (F.drainBy def)

-- Parse and handle commands on a socket

handler :: Socket -> IO ()
handler sk =
  S.unfold NS.read sk
  & IU.decodeLatin1
  & IU.words F.toList
  & S.map (, sk)
  & S.fold demux
  & IM.discard

-- Accept connections and handle connected sockets

server :: IO ()
server =
  serially (S.unfold TCP.acceptOnPort 8091)
  & asyncly . S.mapM (INS.handleWithM handler)
  & S.drain

-- following is client

sender :: SerialT IO ()
sender = S.repeat "time\nrandom\n"
  & IU.unwords IUF.fromList
  & U.encodeLatin1
  & ITCP.transformBytesWith remoteAddr remotePort
  & U.decodeLatin1
  & IU.lines F.drain
  & S.chunksOf chunkSize F.drain

counter :: String -> Int -> () -> IO Int
counter tag n () = do
  let i = n + 1
  when (i `mod` nChunks == 0) $
    putStrLn $ tag ++ show (i * chunkSize)
  return i

nChunks :: Int
nChunks = 10

chunkSize :: Int
chunkSize = 10000

remotePort :: PortNumber
remotePort = 8091

remoteAddr :: (Word8, Word8, Word8, Word8)
remoteAddr = (127, 0, 0, 1)

client :: IO ()
client = S.replicate 4 sender
  & S.concatMapWith async id
  & S.postscanlM' (counter "rcvd: ") 0
  & S.drain

data Args = Client | Server | AcidRain | Unknown
toArgs :: [String] -> Args
toArgs ["Server"] = Server
toArgs ["Client"] = Client
toArgs ["AcidRain"] = AcidRain
toArgs _ = Unknown

-- | following is AcidRain example

data Event = Quit | Harm Int | Heal Int deriving (Show)

userAction :: MonadAsync m => SerialT m Event
userAction = S.repeatM $ liftIO askUser
  where
    askUser = do
      command <- getLine
      case command of
        "potion" -> return (Heal 10)
        "harm" -> return (Harm 10)
        "quit" -> return Quit
        _ -> putStrLn "Type potion or harm or quit" >> askUser

acidRain :: MonadAsync m => SerialT m Event
acidRain = asyncly $ constRate 1 $ S.repeatM $ liftIO $ return $ Harm 1

data Result = Check | Done

runEvents :: (MonadAsync m, MonadState Int m) => SerialT m Result
runEvents = do
  event <- userAction `parallel` acidRain
  case event of
    Harm n -> modify (\h -> h - n) >> return Check
    Heal n -> modify (\h -> h + n) >> return Check
    Quit -> return Done

data Status = Alive | GameOver deriving Eq

getStatus :: (MonadAsync m, MonadState Int m) => Result -> m Status
getStatus result =
  case result of
    Done -> liftIO $ putStrLn "You quit!" >> return GameOver
    Check -> do
      h <- get
      liftIO $ if (h <= 0)
               then putStrLn "You die" >> return GameOver
               else putStrLn ("Health = " <> show h) >> return Alive

myAcidRain :: IO ()
myAcidRain = do
  putStrLn "Your health is deteriorating due to acid rain, type \"option\" or \"quit\""
  let runGame = S.drainWhile (== Alive) $ S.mapM getStatus runEvents
  void $ runStateT runGame 60

main :: IO ()
main = do
  arg <- getArgs
  case toArgs arg of
    Client -> client
    Server -> server
    AcidRain -> myAcidRain
    Unknown -> server
