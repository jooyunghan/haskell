
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r as bs = [area] where
					y x = sum $ zipWith (\a b -> (fromIntegral a)*(x**(fromIntegral b))) as bs
					area = sum [(y x) * 0.001 | x <-[(fromIntegral l)+0.0005, (fromIntegral l)+0.0015..(fromIntegral r)]]

main = putStrLn ""