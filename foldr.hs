foldl' :: (a->b->a) -> a -> [b] -> a
foldl' f a [] = a
foldl' f a (x:xs) = foldl' f (f a x) xs


fix f = f (fix f)

foldr' :: (a->b->b) -> b -> [a] -> b
foldr' cons nil = fix (\f -> \xs -> case xs of
                                          [] -> nil
                                          (y:ys) -> cons y (f ys))