
import Text.ParserCombinators.ReadP
import Control.Applicative
import Text.Read

data BinTree a = Leaf a | Branch (BinTree a) a (BinTree a)

instance (Read a) => Read (BinTree a) where
  readPrec = lift binTree

binTree :: (Read a) => ReadP (BinTree a)
binTree = Branch <$ char '(' <*> binTree <*> readS_to_P reads <*> binTree <* char ')'
          <|> Leaf <$> readS_to_P reads

instance (Show a) => Show (BinTree a) where
  show (Branch left root right) = (showChar '(' . shows left . showChar ' ' . shows root . showChar ' ' . shows right . showChar ')') ""
  show (Leaf root) = show root

t1 :: BinTree Int
t1 = Branch (Leaf 0) 1 (Leaf 2)

t2 :: BinTree Int
t2 = read "(0 1 2)"

newtype Cell = Cell Bool

instance Read Cell where
  readPrec = lift cell

cell :: ReadP Cell
cell = Cell True <$ char 'X' <|> Cell False <$ char '.'

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
