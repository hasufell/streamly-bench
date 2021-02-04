module Main where

import System.Posix.RawFilePath.Directory

import qualified Data.ByteString as BS
import qualified System.Posix.Env.ByteString as Env

main :: IO ()
main = do
  [from, to] <- Env.getArgs
  copyFile from to Strict
