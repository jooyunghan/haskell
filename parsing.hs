-- file parsing.hs

type Parser a = String -> [(a, String)]


g `bind` f = \inp -> case g inp of 
                         [] -> []
                         [(v,out)] -> f v out

item :: Parser Char
item = \inp -> case inp of
                 [] -> []
                 (x:xs) -> [(x, xs)]

failure :: Parser a
failure = \inp -> []

return' :: a -> Parser a
return' v = \inp -> [(v, inp)]

(+++) :: Parser a -> Parser a -> Parser a
p +++ q = \inp -> case p inp of
                       [] -> parse q inp
                       r@[(v,out)] -> r

parse :: Parser a -> String -> [(a, String)]
parse p inp = p inp -- identity

-- -- sequencing example
-- for now >>= or do don't work for type synonyms
-- p :: Parser (Char, Char)
-- p = item `bind` \x ->
--     item `bind` \_ ->
--     item `bind` \y ->
--     return' (x, y)

sat :: (Char -> Bool) -> Parser Char
sat p = item `bind` \x ->
        if p x then return' x
        else failure

digit :: Parser Char
digit = sat isDigit

isDigit x = elem x "0123456789"

many :: Parser a -> Parser [a]
many p = many1 p +++ return' []

many1 :: Parser a -> Parser [a]
many1 p = p `bind` \v ->
          many p `bind` \vs ->
          return' (v:vs)

string :: String -> Parser String
string [] = return' []
string (x:xs) = char x `bind` \_ ->
                string xs `bind` \_ ->
                return' (x:xs)

char :: Char -> Parser Char
char x = sat (== x)
