{-# LANGUAGE FlexibleContexts #-}
import Data.Array.IO
import Data.IORef
import Control.Monad (when, forM_)
import Control.Applicative ((<$>))

readInt :: String -> Int
readInt = read

quicksort (l,r) arr = when (l < r) $ do
  p <- partition (l,r) arr
  quicksort (l,p-1) arr
  quicksort (p+1,r) arr

swap arr i j = do
  a <- readArray arr i
  b <- readArray arr j
  writeArray arr i b
  writeArray arr j a

inc r = do
  v <- readIORef r
  writeIORef r (v+1)

partition (l,r) arr = do
  pivot <- readArray arr r
  ir <- newIORef l
  forM_ [l .. (r-1)] (\j -> do
    i <- readIORef ir
    aj <- readArray arr j
    when (aj <= pivot) (swap arr i j >> inc ir))
  i <- readIORef ir
  swap arr i r
  snapshot <- getElems arr
  putStrLn $ unwords $ map show snapshot
  return i

main = do
  n <- readInt <$> getLine
  ns <- map readInt . words <$> getLine
  arr <- newListArray (1,n) ns :: IO(IOArray Int Int)
  quicksort (1,n) arr
