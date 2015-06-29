module Parsing where


import Data.Char
import Control.Monad

infixr 5 +++

newtype Parser a              =  P (String -> [(a,String)])

instance Monad Parser where
   return v                   =  P (\inp -> [(v,inp)])
   p >>= f                    =  P (\ inp ->  case parse p inp of
                                                   [] -> []
                                                   [(v, out)] -> parse (f v) out)

instance MonadPlus Parser where
   mzero                      =  P (\inp -> [])
   p `mplus` q                =  P (\inp -> case parse p inp of
                                               []        -> parse q inp
                                               [(v,out)] -> [(v,out)])


failure :: Parser a
failure = P (\ inp -> [])

item :: Parser Char
item = P ( \ inp -> case inp of
                      [] -> []
                      (x:xs) -> [(x,xs)])

parse :: Parser a -> String -> [(a,String)]
parse (P p) s = p s


(+++) :: Parser a -> Parser a -> Parser a
p +++ q = p `mplus` q


sat :: (Char -> Bool) -> Parser Char
sat p = do x <- item
           if p x then return x else failure
                      

digit :: Parser Char
digit = sat isDigit

alphanum :: Parser Char
alphanum = sat isAlphaNum

char :: Char -> Parser Char
char c = sat (==c)

string :: String -> Parser String
string [] = return []
string s@(x:xs) = do
    char x
    string xs
    return s

many :: Parser a -> Parser [a]
many p = many1 p +++ return []
            

many1 :: Parser a -> Parser [a]
many1 p = do x <- p
             xs <- many p
             return (x:xs)

nat :: Parser Int
nat = do x <- many1 digit
         return $ read x

int :: Parser Int
int = (do char '-'
          n <- nat
          return $ -n)
      +++ nat

space :: Parser ()
space = do many (sat isSpace)
           return ()

bool :: Parser Bool
bool = do b <- (string "False" +++ string "True")
          return $ read b

comment :: Parser ()
comment = do string "//" 
             many (sat (/= '\n'))
             return ()

type JPair = (String, JValue)

data JValue = JObject [JPair]
            | JString String
            | JNumber Int
            | JArray [JValue]
            | JBool Bool
            | JNull
            deriving (Show)

manywithcomma :: Parser a -> Parser [a]
manywithcomma p = many1withcomma p +++ return []

other :: Parser a -> Parser a
other p = do space
             char ','
             space
             x <- p
             return x

many1withcomma :: Parser a -> Parser [a]
many1withcomma p = do x <- p
                      xs <- many (other p)
                      return (x:xs)

jpair :: Parser JPair
jpair = do char '\"'
           k <- many1 alphanum
           char '\"'
           space
           char ':'
           space
           v <- jvalue
           return (k,v)
           
jobject :: Parser JValue
jobject = do char '{'
             space
             p <- manywithcomma jpair
             space
             char '}'
             return (JObject p)

jstring :: Parser JValue
jstring = do char '\"'
             s <- many (sat (/= '"'))
             char '\"'
             return (JString s)

jnumber :: Parser JValue
jnumber = do i <- int
             return (JNumber i)

jarray :: Parser JValue
jarray = do char '['
            space
            xs <- manywithcomma jvalue
            space
            char ']'
            return (JArray xs)

jbool :: Parser JValue
jbool = do b <- bool
           return $ JBool b

jnull :: Parser JValue
jnull = do string "null"
           return JNull

jvalue :: Parser JValue
jvalue = jobject +++ jstring +++ jnumber +++ jarray +++ jnull +++ jbool
