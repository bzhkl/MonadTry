module MyIO where

import Control.Monad
import Control.Applicative
import Data.Char

type Input      = String
type Remainder  = String
type Output     = String

data MyIO a = MyIO (Input -> (a, Remainder, Output))

apply :: MyIO a -> Input -> (a, Remainder, Output)
apply (MyIO f)  = f


myPutChar :: Char -> MyIO ()
myPutChar      c   = MyIO $ \s -> ((), s, [c])


myPutStr :: String -> MyIO ()
myPutStr  = foldr (>>) (return ()) . map myPutChar

myPutStrLn:: String -> MyIO()
myPutStrLn s = myPutStr s >> myPutChar '\n'

myGetChar :: MyIO Char
myGetChar = MyIO $ \(x : xs) -> (x, xs, "")

myGetLine :: MyIO String
myGetLine = myGetChar >>= \c ->
            if c == '\n' then
              return []
            else
              myGetLine >>= \ xs ->
              return (c:xs)



myEcho :: MyIO ()
myEcho = myGetLine >>= \line ->
         if (line == "") then
           return ()
         else
           myPutStrLn (map toUpper line) >>
           myEcho



instance Functor MyIO where
  fmap = liftM

instance Applicative MyIO where
  pure = return
  (<*>) = ap

instance Monad MyIO where
  return x = MyIO $ \s -> (x, s, "")
  m >>= x   = MyIO $ \s ->
               let (a, rem1, out1) = apply m s        in
               let (b, rem2, out2) = apply (x a) rem1 in
               (b, rem2, out1++out2)


convert :: MyIO () -> IO ()
convert m = interact (\s ->
              let (x, rem, out) = apply m s in
              out)



--apply (myGetchar >>= \x -> myGetChar >>= \y -> return [x,y]) "123"
--  convert (myPutChar 'a')





