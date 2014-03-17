
b = 10

type Bigit = Int
type Vint = [Bigit]


strep :: Vint -> Vint
strep xs = case dropWhile (== 0) xs of 
	[] -> [0]
	ys -> ys

align :: Vint -> Vint -> (Vint, Vint)
align xs ys = let n = length ys - length xs in
	if n > 0 then ((copy 0 n) ++ xs, ys) 
	else (xs, (copy 0 (-n)) ++ ys)

copy x n = [x | i <- [1..n]]

veq = vcompare (==)
vleq = vcompare (<=)
vless = vcompare (<)
vgeq = vcompare (>=)
vgreater = vcompare (>)

vcompare op xs ys = op vs us
	where (us, vs) = align xs ys


carry :: Bigit -> [Bigit] -> [Bigit]
carry x (c:xs) = (x+c) `div` b : (x+c) `mod` b : xs


norm :: Vint -> Vint
norm = strep . foldr carry [0]

vadd xs ys = let (us, vs) = align xs ys in norm (zipWith (+) us vs)
vsub xs ys = let (us, vs) = align xs ys in norm (zipWith (-) us vs)


vnegative = (< 0) . head

vnegate = norm . map negate


psums xs ys = map (bmul xs) ys
bmul xs y = norm (map (* y) xs)

vmul xs ys = foldl1 (\xs ys -> vadd (xs ++ [0]) ys) (psums xs ys)


-- exercises 
-- 4.2.1 define a function absint so that if number x is represented by the list of bigits xs

absint xs = foldl (\a bigit -> a*b + bigit) 0 xs


-- 4.2.2 justify 'vless xs ys = (absint xs < absint ys)'

-- 4.2.3 vnegate = vsub [0] ?

-- 4.2.4 suggest a possible representation for signed-magnitude numbers. re-define vadd and vsub to work with this representation.

-- 4.2.5 suppose inv is a function which converts a string of digit characters to an element of vint. We can define inv by 'inv = pack . map digit' where digit converts a digit character to a decimal digit. and pack converts a list of decimal digits to an element of vint. Define 'digit'. Using 'foldl' and 'vadd', define 'pack'

inv = vpack . map digit

-- converts a digit char to a decimal digit
digit c = fromEnum c - fromEnum '0'

-- converts a list of decimal digits to an element of vint
--
vpack :: [Int] -> Vint
vpack = 

