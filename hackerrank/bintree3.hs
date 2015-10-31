import Text.Read (readPrec, lift)
import Text.ParserCombinators.ReadP (char, skipSpaces)
import Control.Applicative ((<|>))
import GHC.Read (paren)

data BinTree a = Leaf a | Branch (BinTree a) a (BinTree a) deriving Show

instance (Read a) => Read (BinTree a) where
  readPrec = Leaf <$> readPrec
         <|> paren (Branch <$> readPrec <*> readPrec <*> readPrec)

data Cell = On | Off deriving Show

instance Read Cell where
  readPrec = lift $ skipSpaces >> (On <$ char '.' <|> Off <$ char 'X')

t :: BinTree Cell
t = read "(. X    ( X . .))"

main :: IO ()
main = print t
