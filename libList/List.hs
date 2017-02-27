module List where

import Control.Monad
import Control.Applicative
import Data.Char


--data List a = EmptyList
--             | ListElement a (List a)
--instance Functor List where
--  fmap = liftM
--
--instance Applicative List where
--  pure = return
--  (<*>) = ap
--
--
--instance Monad List where
--  return :: a -> List a
--  return x = List x
--
--  (>>=) :: List a-> (a->List b)->List b
--  m >>= k = [y| x <-m, y <- k x]