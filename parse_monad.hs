import Control.Monad

newtype Parser' s a = Parser' { runState :: s -> [(a, s)] }
type Parser = Parser' String

item :: Parser Char
item = Parser' $ \xs -> case xs of 
[] -> []
(y:ys) -> [(y, ys)]

-- (=>>) :: M a -> (a -> M b) -> M b
instance Monad (Parser' s) where
	return x = Parser' $ \xs -> [(x, xs)]
	p1 >>= f = Parser' $ \xs -> case runState p1 xs of
																[] -> []
																[(y, ys)] -> runState (f y) ys

parse :: Parser a -> String -> [(a, String)]
parse p s = runState p s

p :: Parser (Char, Char)
p = do 
	a <- item
	b <- item
	return (a, b)


