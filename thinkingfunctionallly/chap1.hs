
fac 0 = 1
fac n = fac (n-1) * n

double n = n * 2
square n = n * n

squareOfDouble = square . double

pyramid 0 = []
pyramid n = pyramid (n-1) ++ [take' n (repeat' '*')]

repeat' x = x : repeat' x

take' 0 _ = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

len [] = 0
len (_:xs)  = 1 + len xs 

main :: IO () 
main = do
	(putStrLn . unlines . pyramid) 10





