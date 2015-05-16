data Tree a = Empty | Branch a (Tree a) (Tree a)

isValidBST t = isValid t (\_ -> True)
  where 
	isValid Empty _ = True
	isValid (Branch value left right) valid =
	   (valid value) && (isValid left (valid &&& (< value))) && (isValid right (valid &&& (> value)))

v1 &&& v2  = \x -> (v1 x) && (v2 x)

main = putStrLn(show(map isValidBST [t1, t2, t3, t4]))
  where t1 = Branch 3 Empty (Branch 4 Empty (Branch 5 Empty Empty))  -- True
        t2 = Branch 3 (Branch 1 Empty Empty) (Branch 4 Empty Empty)  -- True
        t3 = Branch 3 (Branch 4 Empty Empty) (Branch 4 Empty Empty)  -- False
        t4 = Branch 3 (Branch 3 Empty Empty) Empty                   -- False