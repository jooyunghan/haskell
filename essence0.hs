import Text.ParserCombinators.Parsec
import Control.Monad;

data M a = Success a | Error String

instance Monad M where
	return a = Success a
	(Success a) >>= k = k a
	(Error s) >>= k = Error s

showM :: M Value -> String
showM (Success a) = "Success: " ++ showval a
showM (Error s) = "Error: " ++ s

type Name = String

data Term = Var Name
	| Con Int
	| Add Term Term
	| Lam Name Term
	| App Term Term

data Value = Wrong
	| Num Int
	| Fun (Value -> M Value)

type Environment = [(Name, Value)]

showval :: Value -> String
showval Wrong = "<wrong>"
showval (Num i) = show i
showval (Fun f) = "<function>"

interp :: Term -> Environment -> M Value
interp (Var x) e = lookup' x e
interp (Con i) e = return (Num i)
interp (Add u v) e = do 
	a <- interp u e 
	b <- interp v e
	add a b
interp (Lam x v) e = return (Fun (\a -> interp v ((x,a):e)))
interp (App t u) e = do
	f <- interp t e
	a <- interp u e
	apply f a

lookup' :: Name -> Environment -> M Value
lookup' x [] = return Wrong
lookup' x ((y,b):e) = if x==y then return b else lookup' x e

add :: Value -> Value -> M Value
add (Num i) (Num j) = return (Num (i+j))
add a b = return Wrong

apply :: Value -> Value -> M Value
apply (Fun k) a = k a
apply f a = return Wrong

test :: Term -> String
test t = showM (interp t [])

;;; 

lang :: Parser Term
lang = try addp <|> appl

term :: Parser Term
term = cons <|> var <|> lamb <|> do 
	char '('
	t <- lang
	char ')'
	return t

addp :: Parser Term
addp = chainl1 term (char '+' >> return Add)

cons :: Parser Term
cons = do 
	ds <- many1 digit
	return (Con (read ds))

name :: Parser Name
name = many1 letter

var :: Parser Term
var = do
	n <- name
	return (Var n)

lamb :: Parser Term
lamb = do
	char '\\'
	n <- name
	string "->"
	t <- lang
	return (Lam n t)

appl :: Parser Term
appl = chainl1 term (return App)

eval :: String -> String 
eval input = case (parse term "" input) of
	Left err -> "Error : " ++ show err
	Right x -> test x

