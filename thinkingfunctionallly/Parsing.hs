module Parsing where

import Control.Applicative (Applicative(..), Alternative(..))
import Control.Monad (MonadPlus(..), ap, liftM, guard)
import Data.Char (isLower, isDigit, isSpace)

newtype Parser a = Parser {runParser :: String -> [(a, String)]}

parse :: Parser a -> String -> a
parse p = fst . head . runParser p

instance Functor Parser where
  fmap = liftM
  
instance Applicative Parser where
  pure = return
  (<*>) = ap
  
instance Monad Parser where
  return a = Parser (\s -> [(a, s)])
  p >>= q = Parser (\s -> [(b,s'')|(a,s') <- runParser p s, (b,s'') <- runParser (q a) s'])

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus
  
instance MonadPlus Parser where
  mzero = Parser (\s -> [])
  p `mplus` q = Parser f
                where f s = let result = runParser p s
                            in if null result
                               then runParser q s
                               else result
                           
                   
getc :: Parser Char
getc = Parser f
       where f [] = []
             f (x:xs) = [(x,xs)]
             
sat :: (Char -> Bool) -> Parser Char
sat p = do x <- getc
           guard (p x)
           return x

char x = sat (==x) >> return ()

string [] = return ()
string (x:xs) = char x >> string xs

lower = sat isLower

digit :: Parser Int
digit = do d <- sat isDigit
           return (cvt d)
        where cvt d = fromEnum d - fromEnum '0'

space :: Parser ()
space = many (sat isSpace) >> return ()

symbol s = space >> string s
token p = space >> p

manywith sep p = optional (somewith sep p)
somewith sep p = do x <- p
                    xs <- many (sep >> p)
                    return (x:xs)

optional :: Parser [a] -> Parser [a]
optional p = p <|> return []

int = token (sign <*> nat)
      where sign = char '-' >> return negate <|>
                   return id

nat :: Parser Int
nat = do ds <- many digit
         return (foldl1 shiftl ds)
      where shiftl n d = n*10 + d

bracket p = symbol "[" *> p <* symbol "]"

ints :: Parser [Int]
ints = bracket (manywith (symbol ",") int)
