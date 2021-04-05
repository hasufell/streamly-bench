module Main where

import System.Environment

import qualified Data.ByteString.Lazy as BSL

main :: IO ()
main = do
  [from, to] <- getArgs
  copyFile from to
 where
  copyFile from to = BSL.readFile from >>= BSL.writeFile to
