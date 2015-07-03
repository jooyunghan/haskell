module Main where
import Pretty


shape :: Layout -> [Int]
shape = map length . lines


-- If conditional expression

data CExpr = Expr String | If String CExpr CExpr
cexpr :: CExpr -> Doc
cexpr (Expr s) = text s
cexpr (If p expr1 expr2) = group(group(text "if " <> text p <> line <> text "then " <> nest 5 (cexpr expr1))<> line <> text "else " <> nest 5 (cexpr expr2))

ex1 = If "p" (If "q" (Expr "expr1") (Expr "expr2")) (Expr "expr3")

-- GenTree
data RoseTree a = Node a [RoseTree a] deriving (Eq, Show)

rtree :: (Show a) => RoseTree a -> Doc
rtree (Node a []) = text ("Node " ++ show a ++ " []")
rtree (Node a cs) = group(text ("Node " ++ show a) <> nest 2 (line <> bracket cs))

bracket cs = text "[" <> nest 1 (commas cs) <> text "]"

commas :: (Show a) => [RoseTree a] -> Doc
commas (c:cs) = rtree c <> foldr (<>) nil [text "," <> line <> rtree c | c <- cs]

ex2 = Node 1 [Node 2 [], Node 3 [Node 4 [], Node 5 []]]


-- Paragraphs

para :: String -> Doc
para = cvt . map text . words

cvt [] = nil
cvt (x:xs) = x <> foldr (<>) nil [group (line <> x) | x <- xs]



main :: IO ()
main = do
  print $ pretty 20 $ rtree ex2

   --print $ pretty 30 $ para "This is a fairly short paragraph with just twenty-two words. The problem is that pretty-printing it takes time, in fact 31.32 seconds"


--    mapM_ putStrLn $ layouts $ cexpr $ If "p" (If "q" (Expr "expr1") (Expr "expr2")) (Expr "expr3")
--    print $ map shape $ layouts $ cexpr $ If "p" (If "q" (Expr "expr1") (Expr "expr2")) (Expr "expr3")
    
--quickCheck prop1
