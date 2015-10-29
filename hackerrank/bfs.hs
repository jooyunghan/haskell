import Control.Applicative
import Control.Monad
import Data.List (lookup)
import Data.Array
import qualified Data.Map.Strict as Map

bfs :: Int -> [(Int,Int)] -> Int -> Map.Map Int Int
bfs n edges start = iter Map.empty [(start, 0)]
  where iter :: Map.Map Int Int -> [(Int,Int)] -> Map.Map Int Int
        iter seen [] = seen
        iter seen ((i,d):xs) = case Map.lookup i seen of
          Just d' | d' > d -> iter (Map.insert i d seen) xs
          Just _ -> iter seen xs
          Nothing -> iter (Map.insert i d seen) (xs ++ [(n',d+6)| n' <- neighbors ! i])
        neighbors = accumArray (flip (:)) [] (1,n) $ edges ++ map (\(a,b) -> (b,a)) edges

format :: Int -> Map.Map Int Int -> String
format n dist = unwords [show d| i <- [1..n], let d = Map.findWithDefault (-1) i dist, d /= 0 ]

solve :: IO()
solve = do
  [n,m] <- map read . words <$> getLine
  edges <- replicateM m $ do
    [from,to] <- map read . words <$> getLine
    return (from,to)
  start <- read <$> getLine
  putStrLn $ format n $ bfs n edges start

main :: IO()
main = do
  t <- read <$> getLine
  replicateM_ t solve
