import Text.Read
import Text.ParserCombinators.ReadP
import Control.Applicative
import Control.Monad
import Data.Char

data Expr = Term Int
          | Neg Expr
          | Add Expr Expr
          | Sub Expr Expr
          | Div Expr Expr
          | Mul Expr Expr deriving (Show, Eq)

instance Read Expr where
  readPrec = lift expr

expect :: ReadP a -> ReadP ()
expect = void

token :: String -> ReadP String
token s = skipSpaces >> string s

expr :: ReadP Expr
expr = chainr1 term (Add <$ expect (token "+")  <|>  Sub <$ expect (token "-"))

term :: ReadP Expr
term = chainr1 factor (Mul <$ expect (token "*") <|> Div <$ expect (token "/"))

factor :: ReadP Expr
factor = number
      <|> expect (token "+") *> factor
      <|> Neg <$ expect (token "-") <*> factor
      <|> expect (token "(") *> expr <* expect (token ")")

number :: ReadP Expr
number = Term . read <$> (skipSpaces >> many1 (satisfy isDigit))

p :: Int
p = 10 ^ 9 + 7

eval :: Expr -> Int
eval (Term n) = n
eval (Neg e) = negate $ eval e
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Div e1 e2) = let a = eval e1
                       b = eval e2
                   in if a >= b then a `div` b
                      else a * (pow' b (p - 2) `mod` p)
eval (Mul e1 e2) = eval e1 * eval e2

pow' :: Int -> Int -> Int
pow' a b
  | b == 0 = 1
  | even b =  pow' (pow' a (b `div` 2)) 2 `mod` p
  | otherwise = (pow' (pow' a ((b-1) `div` 2)) 2) * a `mod` p

main :: IO()
main = getLine >>= print . (`mod` p) . eval . read
