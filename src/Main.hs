module Main where
import Hi
import Add
import MyIO


main :: IO ()
main = do
  putStrLn "hello world"
  sayHi
  putStrLn $show $ myAdd 2 3
  convert (myPutChar 'z')
  --convert (myGetChar 'u')
  --convert myEcho