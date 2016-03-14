{-# LANGUAGE FlexibleContexts #-}
module Main where
import Text.Parsec hiding (Empty, (<|>))
import Control.Monad
import Control.Applicative

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

-- stringToTree a(b(d,e),c(,f(g,)))
stringToTree :: Monad m => String -> m (Tree Char)
stringToTree input = do
  Right t <- runParserT treeP () "" input
  return t

treeP :: (Monad m, Stream s m Char) => ParsecT s () m (Tree Char)
treeP =
  uncurry <$> (Branch <$> letter) 
    <*> ((,) <$ char '(' <*> treeP <* char ',' <*> treeP <* char ')' 
         <|> return (Empty, Empty))
  <|> return Empty

treeToPreorder :: Tree a -> [a]
treeToPreorder Empty = []
treeToPreorder (Branch a left right) = a : treeToPreorder left ++ treeToPreorder right

treeToInorder :: Tree a -> [a]
treeToInorder Empty = []
treeToInorder (Branch a left right) = treeToInorder left ++ [a] ++ treeToInorder right

strToTree :: (Monad m) => String -> m (Tree Char)
strToTree input = snd <$> tree input
  where tree "" = return ("", Empty)
        tree [a] = return ("", Branch a Empty Empty)
        tree (a:x:xs) 
          | x == '(' = do (',':ys, left) <- tree xs
                          (')':zs, right) <- tree ys
                          return (zs, Branch a left right)
          | x == ',' || x == ')' = return (x:xs, Branch a Empty Empty)
          | otherwise = fail "Invalid input"

main :: IO ()
main = stringToTree "a(b(d,e),c(,f(g,)))" >>= return.treeToInorder >>= print
