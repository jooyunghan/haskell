
cp :: [[a]] -> [[a]]
cp (xs:xss) =  [y:ys | ys <- cp xss, y <- xs]
cp [] = [[]]

nodups :: (Eq a) => [a] -> Bool
nodups [] = True
nodups (x:xs) = not (x `elem` xs) && nodups xs

type Matrix a = [[a]]

trans :: Matrix a -> Matrix a
trans [] = repeat []
trans (xs:xss) = zipWith (:) xs (trans xss)