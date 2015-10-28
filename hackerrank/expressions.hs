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

p = 10 ^ 9 + 7

eval (Term n) = n
eval (Neg e) = negate $ eval e
eval (Add e1 e2) = eval e1 `madd` eval e2
eval (Sub e1 e2) = eval e1 `msub` eval e2
eval (Div e1 e2) = eval e1 `mdiv` eval e2
eval (Mul e1 e2) = eval e1 `mmul` eval e2

madd a b = (a+b) `mod` p

msub a b = (a-b) `mod` p

mmul a b = (a*b) `mod` p

mdiv a b = rec a b (p - 2)

rec a b e
  | e == 0 = a
  | even e = rec a (mmul b b) (e `div` 2)
  | otherwise = rec (mmul a b) (mmul b b) (e `div` 2)

main :: IO()
main = getLine >>= print . eval . read
