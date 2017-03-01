module Main where

import Data.Char

import Hi
import Add
import MyIO
import Parser
import Control.Monad


main :: IO ()
main = do
  putStrLn "hello world"
  sayHi
  putStrLn $show $ myAdd 2 3
  convert (myPutChar 'z')
  --convert (myGetChar 'u')
  --convert myEcho