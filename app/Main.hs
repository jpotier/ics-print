module Main where

import qualified MyLib (icsPrint)
import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  MyLib.icsPrint path
