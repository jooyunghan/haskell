import Data.Char
import Test.QuickCheck

getList :: IO [Char]
getList = fmap take5 getContents

take5 = take 5 . filter (`elem` ['a' .. 'e'])


