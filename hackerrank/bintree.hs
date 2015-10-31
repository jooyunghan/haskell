import Text.Read as R
import qualified Text.ParserCombinators.ReadP as P (char, skipSpaces)
import Control.Applicative
import GHC.Read

data BinTree a = Leaf a | Branch (BinTree a) a (BinTree a)

char :: Char -> ReadPrec Char
char ch = lift (P.skipSpaces >> P.char ch)

instance (Read a) => Read (BinTree a) where
  readPrec = paren (Branch <$> readPrec <*> readPrec <*> readPrec)
         <|> Leaf <$> readPrec

instance (Show a) => Show (BinTree a) where
  show (Branch left root right) = (showChar '(' . shows left . showChar ' ' . shows root . showChar ' ' . shows right . showChar ')') ""
  show (Leaf root) = show root

t1 :: BinTree Int
t1 = Branch (Leaf 0) 1 (Leaf 2)

t2 :: BinTree Int
t2 = read "(0 1 2)"

newtype Cell = Cell Bool

instance Read Cell where
  readPrec = Cell True <$ char 'X'
         <|> Cell False <$ char '.'

instance Show Cell where
  show (Cell on) = if on then "X" else "."

input :: BinTree Cell
input = read "((X X (X X .)) . ((. X (((. . X) . ((. X ((. X ((. . (((X . (((X . .) X (X . (X X ((X . (X X (((X X ((. X (((X X (((. . X) . (X . .)) X (((. . X) . ((. X X) . (. . (X . ((. X ((((. X (X . ((((. X .) . .) . X) X .))) . (. . (((. X X) X X) . (((X . .) . (. X .)) X X)))) . .) X (X X X))) X ((. . ((X . (((((. X (. X ((X . .) X X))) . (X X X)) X ((. X X) . X)) X ((. . X) X ((. . X) X X))) . ((. X X) . X))) X (. X (X . (((. . X) . X) X .))))) . X)))))) . .))) . .) . .)) X X)) X X) . (. .  ((((((X X (X . X)) . ((X . X) X .)) . .) . .) X X) . (. . X)))))) X (. . (((X . (. X (X . X))) X .) X .)))))) X .)) . (((X X X) X X) . .)) X (X . ((. X .) X (((((((((. . X) X X) X X) X X) X ((. X ((X X (X X ((. . X) X (X X X)))) . .)) . (. X .))) . .) . X) . ((. X X) X (X X X))) X X))))) . (. X .))) X (. X (X X .)))) . ((X X (. . X)) . .))) . (. . (((. . (. X (X X .))) X ((X . (. X (((X . .) . X) . (X . X)))) X X)) X ((X X .) . .))))) . (. X .)))"
--input = read "((X X (X X .)) . X)"

main :: IO ()
main = do
  print t1
  print t2
  print input
