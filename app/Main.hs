{- | This server accepts a stream of command words separated by space characters
   and responds with outputs of the commands.
 -}

{-# LANGUAGE TupleSections #-}

module Main (main) where

import Data.Function ((&))
import Network.Socket (Socket)
import System.Random (randomIO)

import qualified Data.Map.Strict as Map

import Streamly
import qualified Streamly.Prelude as S

import Streamly.Data.Fold (Fold)
import Streamly.Network.Socket
import qualified Streamly.Network.Inet.TCP as TCP

import qualified Streamly.Internal.Data.Time.Clock as Clock
import qualified Streamly.Data.Unicode.Stream as U
import qualified Streamly.Internal.Data.Unicode.Stream as IU
import Streamly.Internal.Network.Socket (handleWithM)

-- Utilities
sendValue :: Show a => Socket -> a -> IO ()
sendValue sk x =
  S.fromList (show x ++ "\n")
  & U.encodeLatin1
  & S.fold (write 60)
  >>= (writeChunks sk)

-- command handlers
time :: Socket -> IO ()
time sk = Clock.getTime Clock.Monotonic >>= sendValue sk

random :: Socket -> IO ()
random sk = (randomIO :: IO Int) >>= sendValue sk

def :: (String, Socket) -> IO ()
def (str, sk) = return ("Unknown command:" ++ str) >>= sendValue sk

commands :: Map.Map String (Fold IO Socket ())
commands = Map.fromList
  [ ("time", drainBy time)
  , ("random", drainBy random)
  ]

demux :: Fold IO (String, Socket) ()
demux = demuxDefault_ commands (drainBy def)

-- Parse and handle commands on a socket

handler :: Socket -> IO ()
handler sk =
  S.unfold readChunksWithBufferOf (32768, sk)
  & U.decodeLatin1
  & IU.words . S.toList
  & S.map (, sk)
  & S.fold demux

-- Accept connections and handle connected sockets

server :: IO ()
server =
  serially (S.unfold TCP.acceptOnPort 8091)
  & asyncly . S.mapM (handleWithM handler)
  & S.drain

main :: IO ()
main = server
