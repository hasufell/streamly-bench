module Main where

import Data.Conduit
import Data.Conduit.Binary

import System.Environment

import qualified Data.ByteString as BS

main :: IO ()
main = do
  [from, to] <- getArgs
  copyFile from to
 where
  copyFile from to = runConduitRes (sourceFile from .| sinkFile to)
