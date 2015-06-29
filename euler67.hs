-- euler67
-- input file: https://projecteuler.net/project/resources/p067_triangle.txt

readInput  = map (map read . words) . lines

solve = foldr1 merge
	where merge xs ys = zipWith (+) xs $ zipWith max ys (tail ys)

main = do
	content <- readFile "p067_triangle.txt"
	--content <- return "3\n7 4\n2 4 6\n8 5 9 3"
	putStrLn $ show $ solve $ readInput content