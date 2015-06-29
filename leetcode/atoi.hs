-- atoi
import Prelude hiding (fail)
import Data.Char
import Control.Monad hiding (guard, fail)
import Data.Maybe

newtype Parser a = Parser (String -> Maybe (a, String))

apply (Parser p) s = p s

item = Parser f 
	where
		f [] = Nothing
		f (x:xs) = Just(x, xs)

instance Monad Parser where
	return x = Parser (\s -> Just (x, s))
	p >>= q = Parser f where
		f s = do 
			(a, s') <- apply p s
			apply (q a) s'

sat :: (Char -> Bool) -> Parser Char
sat p = do
	c <- item
	guard (p c)
	return c

guard True = return ()
guard False = fail

fail :: Parser ()
fail = Parser(\s -> Nothing)

ch :: Char -> Parser ()
ch x = do 
	c <- sat (==x)
	return ()

p <|> q = Parser f
	where f s = 
		case apply p s of
			Just result -> Just result
			_ -> apply q s

many1 :: Parser a -> Parser [a]
many1 p = do
	a <- p
	as <- many p
	return (a:as)

many :: Parser a -> Parser [a]
many p = many1 p <|> return []

getSign:: Parser Int
getSign = 
	(ch '+' >> return 1) <|> (ch '-' >> return (-1)) <|> (return 1)

getDigits :: Parser [Char]
getDigits = many1 (sat isDigit)

number digits = foldl f 0 digits 
	where f z a = z * 10 + (ord a - ord '0')

parse:: Parser Int
parse = do 
		sign <- getSign
		digits <- getDigits
		return $ sign * number digits

trim s = dropWhile isSpace s

atoi :: String -> Int
atoi s = let trimmed = trim s 
	in fromMaybe 0 $ apply parse (trim s) >>= \(a,_) -> return a


main = putStrLn $ show (atoi "   -010")