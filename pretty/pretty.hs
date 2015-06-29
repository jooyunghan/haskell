


{-
24 laws of api

x <> (y <> z) = (x <> y) <> z
x <> nil = x
nil <> x = x

text (s ++ t) = text s <> text t
text "" = nil

layouts nil = [""]
layouts (text s) = [s]
layouts line = ["\n"]
layouts (x <> y) = layouts x <++> layouts y
layouts (nest i x) = map (nestl i) (layouts x)
layouts (group x) = layouts (flatten x) ++ layouts x

nest 0 x = x
nest i nil = nil
nest i (text s) = text s
nest i line = line <> text (replicate i ' ')
nest i (x <> y) = nest i x <> nest i y
nest i (nest j x) = nest (i+j) x
nest i (group x) = group (nest i x)

flatten nil = nil
flatten (text s) = text s
flatten line = text ' '
flatten (x <> y) = flatten x <> flatten y
flatten (nest i x) = flatten x
flatten (group x) = flatten x

-}

type Doc = [Layout]
type Layout = String

pretty :: Int -> Doc -> String
pretty = undefined

layouts :: Doc -> [Layout]
layouts = id

nil :: Doc
nil = [""]

line :: Doc
line = ["\n"]

text :: String -> Doc
text = (:[])

(<>) :: Doc -> Doc -> Doc
(<>) = (<++>)

nest :: Int -> Doc -> Doc
nest i = map (nestl i)

group :: Doc -> Doc
group x = flatten x ++ x

flatten :: Doc -> Doc
flatten x = [flattenl (head x)]

---

xs <++> ys = [x++y | x<-xs, y<-ys]

nestl i xs = concat [p x | x<-xs]
 where p x = if x == '\n' then '\n':replicate i ' ' else [x]

flattenl [] = []
flattenl (x:xs) = if x == '\n' then ' ' : flattenl (dropWhile (==' ') xs) else x : flattenl xs

---

shape :: Layout -> [Int]
shape = map length . lines

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
    print $ map shape $ layouts $ cexpr c1
--  putStrLn $ pretty 30 $ para pg

