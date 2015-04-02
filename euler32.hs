module Main where

import Data.List

answer ra rb = [a*b| 
	a<- ra, 
	b<- rb, 
	pandigits (show a ++ show b ++ show (a*b))]

pandigits as = length as == 9 
	&& sort as == "123456789"

main = 
    putStrLn $ show . sum . nub $ answer [1..9] [1000..9999] ++ answer [10..99] [100..999]
