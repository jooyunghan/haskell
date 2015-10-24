main = getLine >> interact (show . p . read)
       where p :: Int -> Int
             p n = n * (1 + 3 * n - 2) `div` 2
