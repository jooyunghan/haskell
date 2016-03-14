import Control.Applicative
import Data.Ratio
import Control.Monad

rdigits n
 | n < 10 = [n]
 | otherwise = let (d,m) = divMod n 10 in m : rdigits d

bouncy n = bouncy' (rdigits n)

bouncy' ds = any g $ scanl f 0 $ zip ds (tail ds)
  where f z (a,b) = if a == b then z
                    else if a > b
                         then (if z == 1
                               then (-2)
                               else -1)
                          else (if z == (-1)
                               then (-2)
                               else 1)
        g x = x == (-2)

str = scanl f (1,0) [2..]
      where f (a,b) i = if bouncy i then (i, b+1) else (i,b)

solve':: Integer -> Integer -> Integer
solve' n m =
  fst $ head $ filter g str
  where
        g (a,b) = ratio <= (b % a)
        ratio = n % m

solve = do
  [n,m] <- fmap read . words <$> getLine
  print $ solve' n m

main = do
  t <- read <$> getLine
  replicateM_ t solve
