
import Pretty


-- Conditional expression --

data CExpr = Expr String | If String CExpr CExpr

cexpr :: CExpr -> Doc
cexpr (Expr s) = text s
cexpr (If p e1 e2) = group (group (text ("if " ++ p) <> line
             <> text "then " <> nest 5 (cexpr e1)) <> line
             <> text "else " <> nest 5 (cexpr e2))

c1 = If "wealthy" (If "happy" (Expr "lucky you") (Expr "tough")) (If "in love" (Expr "content") (Expr "miserable"))


-- GenTree --
data GenTree a = Node a [GenTree a]

gtree :: (Show a) => GenTree a -> Doc
gtree (Node a []) = text ("Node " ++ show a ++ " []")
gtree (Node a cs) = text ("Node " ++ show a) <> group (nest 2 (line <> bracket cs))

bracket cs = text "[" <> nest 1 (commas cs) <> text "]"

commas (c:cs) = gtree c <> foldr (<>) nil [text "," <> line <> gtree c | c<-cs]

{-
Node a [c, c, c]
Node a
  [c,
   c,
   c]
-}

t1 = Node 1[Node 2[Node 7 [], Node 8[]], Node 3[Node 9[Node 10[], Node 11[]]], Node 4[], Node 5[Node 6[]]]

-- Paragraph --
para :: String -> Doc
para = cvt . map text . words

cvt [] = nil
cvt (w:ws) = w <> foldr (<>) nil [group (line <> w)| w <- ws]

pg = "This is a pretty-printer written in Haskell.\n\
    \This will demonstrate how to write a library.\n\
    \Also this can be a nice demonstration of how to apply\n\
    \techniques introduced in previous chapters, such as\n\
    \Tupling and Accumulators."


main = do
--  mapM_ putStrLn $ layouts $ cexpr c1 
--    print $ map shape $ layouts $ cexpr c1
  putStrLn $ pretty 30 $ para pg

