import Data.List
import Data.Array

wordBreak2 s dic = map unwords $ answers ! 0 
    where answers = listArray (0, length s) $ map f [0..length s - 1] ++ [[[]]]
          f n = [ w : sub | w <- dic, isPrefixOf w (drop n s), sub <- answers ! (n + length w)]

main = putStrLn (show (wordBreak2 "catsanddogs" ["cat", "cats", "and", "sand", "dog", "dogs"]))


