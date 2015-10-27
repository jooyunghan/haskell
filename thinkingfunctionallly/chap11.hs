module Parsing where

import Control.Applicative
import Control.Arrow
import Control.Monad hiding (fail)
import Data.Char
import Prelude hiding (fail)

newtype Parser a = Parser (String -> [(a, String)])

runParser (Parser p) s = p s

instance Monad Parser where
  return a = Parser (\s -> [(a, s)])
  p >>= q = Parser $ \s -> runParser p s >>=  uncurry runParser . first q

fail = Parser (\s -> [])
  
instance Applicative Parser where
  pure  = return
  (<*>) = ap

instance Functor Parser where
  fmap = liftM

instance Alternative Parser where
  empty = mzero
  (<|>) = mplus

instance MonadPlus Parser where
  mzero = fail
  p `mplus` q = Parser (\s -> let results = runParser p s
                              in if null results
                                 then runParser q s
                                 else results)
                                   
getc :: Parser Char
getc = Parser f
       where f (x:xs) = [(x, xs)]
             f [] = []

sat :: (Char -> Bool) -> Parser Char
sat cond = do x <- getc
              guard (cond x)
              return x

char :: Char -> Parser ()
char c = sat (== c) >> return ()

string :: String -> Parser ()
string [] = return ()
string (x:xs) = char x >> string xs >> return ()

space = many (sat isSpace)
symbol s = space >> string s
bracket p = symbol "[" *> p <* symbol "]"

main :: IO ()
main = print "Hello"
