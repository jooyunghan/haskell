import Control.Monad
import Data.Array

data Tree = Empty | Tree Int Tree Tree deriving (Show, Eq)

getInt :: IO Int
getInt = read <$> getLine

getInts :: IO [Int]
getInts = (map read.words) <$> getLine

makeTree :: Int -> [[Int]] -> Tree
makeTree size children = makeTree' 1
                          where makeTree' (-1) = Empty
                                makeTree' n = let [left,right] = arr ! n
                                              in Tree n (makeTree' left) (makeTree' right)
                                arr = listArray (1,size) children
swap :: Tree -> Int -> Tree
swap t k = swap' t 1
           where
              swap' Empty _ = Empty
              swap' (Tree n left right) d = let left' = swap' left (d+1)
                                                right' = swap' right (d+1)
                                            in if d `mod` k == 0 then Tree n right' left'
                                               else Tree n left' right'

inorder :: Tree -> [Int]
inorder Empty = []
inorder (Tree n left right) = inorder left ++ [n] ++ inorder right

main :: IO()
main = do
  n <- getInt
  tree <- makeTree n <$> replicateM n getInts
  t <- getInt
  ks <- replicateM t getInt
  let trees = tail $ scanl swap tree ks
  forM_ trees (putStrLn.unwords.map show.inorder)
