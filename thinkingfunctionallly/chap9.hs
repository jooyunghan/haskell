

-- primes = [2..] \\ composites
-- composites = mergeAll multiples
-- multiples = [map (n*) [n..]| n <- [2..]]

(\\) :: (Ord a) => [a] -> [a] -> [a]
(x:xs) \\ (y:ys) | x > y = (x:xs) \\ ys
                 | x == y = xs \\ ys
                 | x < y = x : (xs \\ (y:ys))

mergeAll :: (Ord a) => [[a]] -> [a]
mergeAll = foldr1 xmerge
xmerge (x:xs) ys = x:merge xs ys

merge :: (Ord a) => [a] -> [a] -> [a]
merge (x:xs) (y:ys) | x<y  = x:merge xs (y:ys)
                    | x==y = x:merge xs ys    -- remove duplicate
                    | x>y  = y:merge (x:xs) ys

pirmes = 2: ([3..] \\ composites)
  where
    composites = mergeAll [map (p*) [p..] | p <- primes]

main :: IO ()
main = do
  print "hello"
