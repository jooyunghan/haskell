import Text.Read
import Text.ParserCombinators.ReadP
import Control.Monad
import Control.Applicative
import Data.Array
import Debug.Trace (trace)

input0 = "5 3\n2 4 -10 2 -2"
input1 = "4 2\n-2 5 -1 -8"
input3 = "100 16\n9404 8036 -9334 -9146 8085 3024 988 5875 2264 -4643 -8916 -8072 1954 3424 5364 -2633 -8910 -7310 9443 -5096 4982 -7834 5164 -8360 185 265 277 -4154 -6615 6233 5988 -9008 5849 -948 6458 -9633 7955 432 1308 6533 -4667 9545 9446 1002 4452 -2285 -2413 -8734 4224 5492 -6250 -38 -3089 -6761 2326 -2209 -7962 -929 5710 -391 -6415 5399 4758 933 -3318 -8572 566 8181 -1512 -2937 -5897 5525 -7054 912 -8863 -4893 2963 -8827 -8376 5579 -8906 2265 5349 9388 4664 5708 2630 -4177 7665 6774 -5152 -5504 6138 2018 2464 3936 -5985 9804 -9520 1245"
output3 = [47985,40945,20236,17440,11090,10742,9716,9443,8747,5710,5579,5525,5164,4982,2963,2326]

data Input = Input Int Int [Int]

instance Read Input where
  readPrec = do
    n <- readPrec
    k <- readPrec
    list <- replicateM n readPrec
    return $ Input n k list

solve :: (Int,Int) -> Array Int Int -> [Int]
solve (l,r) arr
  | l > r = []
  | otherwise = let (sum', (l', r')) = greatestSubarray (l,r) arr
                  in sum' : merge (solve (l, l'-1) arr) (solve (r'+1,r) arr)

greatestSubarray :: (Int,Int) -> Array Int Int -> (Int, (Int, Int))
greatestSubarray (l,r) arr = snd$foldl f ((0,(l,l)), (0,(l,r))) [l..r] where
                              f ((maxEnd,(l,_)), soFar) i = (a,b) where
                                a = max' (0,(i+1,i+1)) (maxEnd + (arr!i), (l,i))
                                b = max' soFar a

max' (s,(l,r)) (t,(i,j))
  |  s > t = (s,(l,r))
  |  t > s = (t, (i,j))
  |  l < i = (s,(l,r))
  |  i > l = (t, (i,j))
  |  r < j = (s,(l,r))
  |  otherwise = (t, (i,j))

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] a = a
merge a [] = a
merge (a:as) (b:bs) = if a > b then a : merge as (b:bs) else b : merge (a:as) bs

solve' :: String -> [Int]
solve' input = let Input n k list = read input in take k $ takeWhile (>0) $ solve (1,n) (listArray (1,n) list)

main :: IO()
main = do
  input <- getContents
  let sums = solve' input
  forM_ sums print
