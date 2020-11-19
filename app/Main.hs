module Main (main) where

import ExploreStreamly (someFunc)

import qualified Streamly.Prelude as S
import Data.Function ((&))

main :: IO ()
main = S.drain $
  S.repeatM getLine
  & fmap (read :: String -> Int)
  & S.filter even
  & S.takeWhile (<= 9)
  & fmap (\x -> x * x)
  & S.mapM print
