import Data.List
import Data.Maybe

-- convert 308000
--; three hundred and eight thousand
--; convert 369027
--; three hundred and sixty-nine thousand and twenty-seven
--; convert 369401
--; three hundred and sixty-nine thousand four hundred and one


convert2 n = combine2 (digits2 n)
digits2 n = (n `div` 10, n `mod` 10)

units = ["one", "two", "three", "four", "five",
    "six", "seven", "eight", "nine"]
teens = ["ten", "eleven", "twelve", "thirteen", "fourteen",
    "fifteen", "sixteen", "seventeen", "eighteen", "nineteen"]
tens = ["twenty", "thirty", "forty", "fifty",
    "sixty", "seventy", "eighty", "ninety"]

combine2 (0, u) |  u > 0 = units !! (u - 1)
combine2 (1, u) = teens !! u
combine2 (t, 0) | t > 1 = tens !! (t - 2)
combine2 (t, u) | t > 1 && u > 0 = tens !! (t - 2) ++ "-" ++ units !! (u - 1)

convert3 n = combine3 (digits3 n)
digits3 n = (n `div` 100, n `mod` 100)

combine3 (0, t) = convert2 t
combine3 (h, 0) | h > 0 = units !! (h - 1) ++ " hundred"
combine3 (h, t) | h > 0 && t > 0 = units !! (h - 1) ++ " hundred and " ++ convert2 t 

convert6 n = combine6 (digits6 n)
digits6 n = (n `div` 1000, n `mod` 1000)

combine6 (0, h) | h > 0 = convert3 h
combine6 (m, 0) | m > 0 = convert3 m ++ " thousand"
combine6 (m, h) | m > 0 && h > 0 = convert3 m ++ " thousand" ++ link h ++ convert3 h

link h | h < 100 = " and "
       | otherwise = " "

-- convert = convert6

-- 4.1.1 full stop char
-- convert = (++ ".") . convert6 

-- 4.1.2 generalize solution up to one billion (1,000,000,000)
convert9 n = combine9 (digits9 n)
digits9 n = (n `div` 1000000, n `mod` 1000000)

combine9 (0, h) | h > 0 = convert6 h
combine9 (m, 0) | m > 0 = convert3 m ++ " million"
combine9 (m, h) | m > 0 && h > 0 = convert3 m ++ " million" ++ link h ++ convert6 h

-- convert = (++ ".") . convert9 

-- 4.1.3 negative

convert n = sign n ++ convert9 (abs n) ++ "."
sign n | n >= 0 = ""
       | otherwise = "minus "

-- 4.1.4 pence 
-- pence 3649 = "thirty-six pounds and forty-nine pence"

pence n = combine_pence (digits_pence n)
digits_pence n = (n `div` 100, n `mod` 100)

combine_pence (0, p) | p > 0 = convert9 p ++ " pences"
combine_pence (pound, 0) = convert9 pound ++ " pounds"
combine_pence (pound, p) = convert9 pound ++ " pounds and " ++ convert9 p ++ " pences"

-- 4.1.5 inverse of convert
-- iconvert "twenty-three" = 23


iconvert str = foldl combine_s 0 $ words (replace '-' ' ' str)

combine_s a s | s == "hundred" = let h = a `mod` 10 in a - h + h * 100
              | s == "thousand" = let t = a `mod` 1000 in a - t + t * 1000
              | s == "million" = let m = a `mod` 1000 in a - m + m * 1000000
              | elem s units = let u = fromJust (elemIndex s units) in a + u + 1
              | elem s teens = let t = fromJust (elemIndex s teens) in a + t + 10
              | elem s tens = let t = fromJust (elemIndex s tens) in a + (t + 2) * 10
              | otherwise = a

replace a b list = [ c | x <- list, let c = if x == a then b else x ]



