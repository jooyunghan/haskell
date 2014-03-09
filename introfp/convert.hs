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

convert = convert6
