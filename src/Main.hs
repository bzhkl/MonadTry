module Main where

import Data.Char

import Hi
import Add
import MyIO
import Parser
import Control.Monad
import System.Random
import Control.Exception.Base

type Size    = Int


main :: IO ()
main = do
  putStrLn "hello world"
  sayHi
  putStrLn $show $ myAdd 2 3
  convert (myPutChar 'z')
  --convert (myGetChar 'u')
  --convert myEcho



scale :: Size -> (Int, Int)
scale s = let n = toInteger (31 * s `div` 100) in
            (fromInteger (-2^n), fromInteger (2^n - 1))


m = snd (head l)

l = [(3, a>>=b), undefined, (8, putStrLn "a")]

a = getLine
b = fst (putStrLn, putStrLn)



first= getLine

rest v = putStrLn v

prog = fmap rest first


continue ::IO()
continue = do
  a <- prog
  a

echoNonFRP = eventLoop $ putStrLn . ("You typed: "++)



eventLoop fire = loop
   where
   loop = do
      s <- getLine
      fire s
      when (s /= "quit") loop



















