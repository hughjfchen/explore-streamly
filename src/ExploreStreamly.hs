{- |
Copyright: (c) 2020 Hugh JF Chen
SPDX-License-Identifier: MIT
Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>

To explore the streamly concurrent streaming framework.
-}

module ExploreStreamly
       ( someFunc
       ) where

someFunc :: IO ()
someFunc = do
  putStrLn ("someFunc" :: String)
  putStrLn "Wow"
  putStrLn "This is a wow message!"
