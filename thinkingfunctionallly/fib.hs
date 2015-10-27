module Main where

{-# LANGUAGE RankNTypes #-}

import Control.Monad.ST (ST, runST)
import Data.STRef (newSTRef, readSTRef, writeSTRef)

{-
fib :: Int -> Integer
fib n = fst (fib2 n)
fib2 0 = (0, 1)
fib2 n = (b, a+b) where (a, b) = fib2 (n-1)
-}

fibST :: Int -> ST s Integer
fibST n = do
  a <- newSTRef 0
  b <- newSTRef 1
  repeatFor n $ do
    x <- readSTRef a
    y <- readSTRef b
    writeSTRef a y
    writeSTRef b $! (x+y)
  readSTRef a

repeatFor :: (Monad m) => Int -> m a -> m ()
repeatFor n ma = foldr (>>) done $ replicate n ma

done :: (Monad m) => m ()
done = return ()

fib :: Int -> Integer
fib n = runST (fibST n)

qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort (x:xs) = qsort [y | y <- xs, y < x] ++ [x] ++
               qsort [y | y <- xs, x <= y]


applyToPair :: (forall a. [a] -> d) -> ([b], [c]) -> (d, d)
applyToPair f (a, b) = (f a, f b)

main :: IO ()
main = do
  print $ fib 100
  print $ applyToPair length ("Hello", [1..10])
