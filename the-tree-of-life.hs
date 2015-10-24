import Text.Read
import Data.Monoid
import Data.Bits

data Cell = On | Off deriving (Eq)
instance Show Cell where
 show On = "X"
 show Off = "."
instance Read Cell where
 readsPrec _ r = [(On, r') | ("X", r') <- lex r]
                ++ [(Off, r') | (".", r') <- lex r]
instance Monoid Cell where
 mempty = Off
 Off `mappend` Off = Off
 mappend _ _ = On

data BinTree a = Leaf a | BinTree (BinTree a) a (BinTree a)
instance (Show a) => Show (BinTree a) where
 show (BinTree left root right) = "(" ++ show left ++ " " ++ show root ++ " " ++ show right ++ ")"
 show (Leaf cell) = show cell

instance (Read a) => Read (BinTree a) where
 readsPrec _ r = [(Leaf cell,r') | (cell,r') <- reads r]
                 ++ readParen True (\r -> [(BinTree left root right,r''') | (left,r') <- reads r,
                                                                            (root,r'') <- reads r',
                                                                            (right,r''') <- reads r''] ) r
value ::  BinTree a -> a
value (Leaf a) = a
value (BinTree _ a _) = a

instance Functor BinTree where
 fmap f (Leaf a) = Leaf (f a)
 fmap f (BinTree left root right) = BinTree (fmap f left) (f root) (fmap f right)

type Rule = Int

binValue :: (Cell, Cell, Cell, Cell) -> Int
binValue (a,b,c,d) =  (if a == On then 8 else 0)
                  .|. (if b == On then 4 else 0)
                  .|. (if c == On then 2 else 0)
                  .|. (if d == On then 1 else 0)

runCA :: Rule -> BinTree Cell -> Int -> BinTree Cell
runCA _ t 0 = t
runCA r t n = runCA r (applyCA r t) (n - 1)

applyCA :: Rule -> BinTree Cell -> BinTree Cell
applyCA r t = fmap (applyRule r) (neighbors t)

applyRule :: Rule -> (Cell, Cell, Cell, Cell) -> Cell
applyRule r vec = if testBit r (binValue vec) then On else Off

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
          loop rule tree n
          where loop :: Rule -> BinTree Cell -> Int -> IO ()
                loop _ _ 0 = return ()
                loop r t n = do q <- getLine >>= return. read
                                let t' = runCA r t (steps q)
                                putStrLn $ show $ query t' (path q)
                                loop r t' (n-1)
