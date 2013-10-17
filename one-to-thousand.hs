import Data.Char
oneToNine = ["one","two","three","four","five","six","seven","eight","nine"]
tenToNineteen = ["ten","eleven","twelve","thirteen","fourteen","fifteen","sixteen","seventeen","eighteen","nineteen"]
twentyToNinety = ["twenty","thirty","forty","fifty","sixty","seventy","eighty","ninety"]

oneToNinetyNine = oneToNine ++ tenToNineteen ++ [x++y | x<-twentyToNinety, y<-(""):oneToNine]

oneToNineHundredNinetyNine = oneToNinetyNine ++ [x++"hundred"++y | x<-oneToNine,y<-(""):(map (\a -> "and"++a) oneToNinetyNine)]

oneThousand = oneToNineHundredNinetyNine ++ ["onethousand"]
 
sol = foldr (+) 0 (map length oneThousand)

countLetters = length . filter isLetter
tens n = (words "ignore ignore twenty thirty forty fifty sixty seventy eighty ninety") !! n
hundreds n = toEnglish n ++ "hundred"
toEnglish n
  | n == 1000 = "thousand"
  | n <= 19 = ("" : words "one two three four five six seven eight nine ten eleven twelve thirteen fourteen fifteen sixteen seventeen eighteen nineteen") !! n
  | n < 100 = tens (n `quot` 10) ++ "" ++ toEnglish (n `mod` 10)
  | n `mod` 100 == 0 = hundreds (n `quot` 100)
  | n < 1000 = hundreds (n `quot` 100) ++ "and" ++ toEnglish (n `mod` 100)

