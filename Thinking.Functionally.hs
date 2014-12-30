
import Prelude hiding (exp)
import Data.Maybe
import Data.Char

-- Ch. 2, Ex. F -- Exponential
exp :: Integer -> Integer -> Integer
exp x n | n == 0    = 1
        | n == 1    = x
        | otherwise = x * exp x (n - 1)

-- log(n)
exp2 :: Integer -> Integer -> Integer
exp2 x n | n == 0 = 1
         | n == 1 = x
         | even n = exp2 (x * x) m
         | odd n  = x * exp2 (x * x) m
         where m = n `div` 2

-- Ch. 2, Ex. G -- ShowDate
showDate :: (Int, Int, Int) -> String
showDate (day, month, year) = show day ++ suffix day ++ " " ++ showMonth month ++ "," ++ show year
	where suffix = fromMaybe "th" . flip lookup [(1, "st"), (2, "nd"), (3, "rd")] . (`mod` 10)
	      showMonth = (!!) [undefined, "January", "February", "March", "April", "May", "June",
	                       "July", "August", "September", "October", "November", "December"]

-- Ch. 2, Ex. H -- Checksum Card Identification Number
type CIN = String

getDigit :: Char -> Int
getDigit c = read [c]

addSum :: CIN -> CIN
addSum cin = cin ++ show (n `div` 10) ++ show (n `mod` 10)
  where n = sum (map getDigit cin)

valid :: CIN -> Bool
valid cin = cin == addSum (take 8 cin)

-- Ch. 2, Ex. I -- Palindrome
palindrome :: IO ()
palindrome = do 
	putStrLn "Enter a string:" 
	s <- getLine 
	putStrLn $ if isPalindrome s then "Yes!" else "No!" 

isPalindrome :: String -> Bool
isPalindrome xs = ys == reverse ys
  where ys = map toLower (filter isAlpha xs)

main = palindrome