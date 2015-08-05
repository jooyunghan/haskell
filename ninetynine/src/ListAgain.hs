module ListAgain where

{-
P21> insertAt 'X' "abcd" 2
"aXbcd"
-}

insertAt :: a -> [a] -> Int -> [a]
insertAt e xs 0 = e:xs
insertAt e (x:xs) n = x : insertAt e xs (n-1)


