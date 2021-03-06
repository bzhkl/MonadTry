module Parser where

import Control.Applicative
import Control.Monad
import Data.Char

newtype Parser a = Parser (String -> [(a, String)])


apply :: Parser a -> String -> [(a, String)]
apply (Parser f) s = f s


parse :: Parser a -> String -> a
parse m s = one [x | (x, t) <- apply m s, t== ""]
  where
  one [] = error "no parse"
  one [x] = x
  one xs | length xs > 1 = error "ambiguous parse"


instance Functor Parser where
  fmap = liftM

instance Applicative Parser where
  pure = return
  (<*>) = ap

instance Monad Parser where
  return x = Parser (\s->[(x,s)])
  m >>= k = Parser( \s->
                        [ (y, u) |
                          (x, t) <-apply m s,
                          (y, u) <-apply (k x) t
                           ])




instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  mzero = Parser (\s-> [])
  mplus m n = Parser (\s -> apply m s ++ apply n s)


char :: Parser Char
char = Parser$ \s ->
           case s of
             [] -> mzero
             (x:xs) -> return (x,xs)

--Parser.apply char "aaa"




spot::(Char -> Bool) -> Parser Char
spot f = char >>= \c -> guard (f c) >>= \_->return c

--Parser.apply (spot isDigit) "123"



parseDigit :: Parser Char
parseDigit = spot isDigit



token:: Char -> Parser Char
token c = spot (==c)

--Parser.apply (token 'a') "123"


addsth :: Parser String
--addsth = spot isDigit >>= \a->return (a:[])
addsth = spot isDigit >>= \a->token '+'>>= \b->spot isDigit >>= \c-> return $show a ++ "+" ++ show c

--Parser.apply addsth "1+2ddd"

matchEx2 :: String -> Parser String
matchEx2 (x:xs) = do
  y <- token x
  ys <- match xs
  return $y : ys


matchEx :: String -> Parser String
matchEx s = sequence (map token s)

-- Parser.apply (matchEx "aa") "aa123"

match :: String -> Parser String
match = mapM token

-- Parser.apply (matchEx2 "aa") "aa123"

test :: Parser [String]
test =
  return []

--Parser.apply  test "123"


star :: Parser a -> Parser [a]
star p = plusEx p `mplus` return []
-- Parser.apply (star $ spot isDigit) "123"


plus :: Parser a -> Parser[a]
plus p =
  p >>= \x -> star p >>= \xs -> return $ x : xs

-- Parser.apply (plus $ spot isDigit) "123"
-- parse (plus $ spot isDigit) "123"


plusEx :: Parser a -> Parser [a]
plusEx a = do
  x <- a
  xs <- star a
  return $ x:xs
-- Parser.apply (plusEx $ spot isDigit) "123"




--Parser.apply (star $ token 'a' `mplus` token 'b') "a1234"


parseNat :: Parser Int
parseNat = plus parseDigit >>= \s-> return $ read s

--Parser.apply parseNat "1234"


parseNeg :: Parser Int
parseNeg = token '-' >> parseNat >>= \n -> return $ -n

--Parser.apply parseNat "1234"


parseInt :: Parser Int
parseInt = parseNat `mplus`  parseNeg
--Parser.apply parseInt "-1234"
--Parser.apply parseInt "1234"










