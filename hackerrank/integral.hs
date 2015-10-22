import Text.Printf (printf)

pow a b = if b >= 0 then a ^ b else 1 / a ^ (abs b)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = [area, volume]
                   where xs = takeWhile (< fromIntegral r) $ iterate (+ 0.001) (fromIntegral l + 0.0005)
                         f x = sum $ zipWith (\a' b' -> fromIntegral a' * pow x b' ) a b
                         area = sum $ map (\x -> f x * 0.001) xs
                         volume = sum $ map (\x -> pi * f x ^ 2 * 0.001) xs

--Input/Output.
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines
