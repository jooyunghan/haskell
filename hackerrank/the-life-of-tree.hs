-- Enter your code here. Read input from STDIN. Print output to STDOUTimport Text.Read
import Data.Monoid
import Data.Bits
import Data.Array
import Text.Read

newtype Cell = Cell Char deriving (Eq)
on = Cell 'X'
off = Cell '.'

instance Show Cell where
 show (Cell x) = x:[]

instance Read Cell where
 readsPrec _ r = [(Cell x, r') | (x:[], r') <- lex r, x == '.' || x == 'X']

instance Monoid Cell where
 mempty = off
 c1 `mappend` c2 = if c1 == off && c2 == off then off else on

data BinTree a = Leaf !a | BinTree !(BinTree a) !a !(BinTree a)
instance (Show a) => Show (BinTree a) where
 show (BinTree left root right) = "(" ++ show left ++ " " ++ show root ++ " " ++ show right ++ ")"
 show (Leaf cell) = show cell

instance (Read a) => Read (BinTree a) where
 readsPrec _ r = [(BinTree left root right,r''''') | ("(", r') <- lex r,
                                                     (left,r'') <- reads r',
                                                     (root,r''') <- reads r'',
                                                     (right,r'''') <- reads r''',
                                                     (")", r''''') <- lex r'''']
                 ++ [(Leaf cell,r') | (cell,r') <- reads r]
value ::  BinTree a -> a
value (Leaf a) = a
value (BinTree _ a _) = a

instance Functor BinTree where
 fmap f (Leaf a) = Leaf (f a)
 fmap f (BinTree left root right) = BinTree (fmap f left) (f root) (fmap f right)

type Rule = Int

binValue :: (Cell, Cell, Cell, Cell) -> Int
binValue (a,b,c,d) =  (if a == on then 8 else 0)
                  .|. (if b == on then 4 else 0)
                  .|. (if c == on then 2 else 0)
                  .|. (if d == on then 1 else 0)

applyCA :: Rule -> BinTree Cell -> BinTree Cell
applyCA r t = fmap (applyRule r) (neighbors t)

applyRule :: Rule -> (Cell, Cell, Cell, Cell) -> Cell
applyRule r vec = if testBit r (binValue vec) then on else off

neighbors :: (Monoid a) => BinTree a -> BinTree (a, a, a, a)
neighbors t = neighbors' mempty t

neighbors' :: (Monoid a) => a -> BinTree a -> BinTree (a,a,a,a)
neighbors' parent (BinTree left root right) = BinTree (neighbors' root left) (parent,(value left),root,(value right)) (neighbors' root right)
neighbors' parent (Leaf cell) = Leaf (parent, mempty, cell, mempty)

type Path = String
data Query = Query {steps :: Int, path :: Path} deriving Show
instance Read Query where
 readsPrec _ r = [(Query steps path, r'''') | (steps, r') <- reads r,
                                              (Punc "[", r'') <- reads r',
                                              (Symbol path, r''') <- reads r'',
                                              (Punc "]", r'''') <- reads r''']
                ++ [(Query steps [], r''') | (steps, r') <- reads r,
                                            (Punc "[", r'') <- reads r',
                                            (Punc "]", r''') <- reads r'']
query :: BinTree a -> Path -> a
query t [] = value t
query (BinTree left root right) (x:xs)
  | x == '>' = query right xs
  | otherwise = query left xs

main = do rule <- getLine >>= return. read
          tree <- getLine >>= return. read
          n <- getLine >>= return. read
          loop n 0 (trees rule tree)
          where loop 0 _ _ = return ()
                loop n s trees = do q <- getLine >>= return. read
                                    let s' = s + (steps q)
                                    putStrLn $ show $ query (trees ! s') (path q)
                                    loop (n-1) s' trees
                trees rule tree = listArray (0,1000) $ iterate (applyCA rule) tree
