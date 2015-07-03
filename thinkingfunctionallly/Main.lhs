> module Main where
> import Data.Char
> import Test.QuickCheck
> import Test.QuickCheck.Modifiers

problem is pretty printing.

1)
if p then expr1 else expr2

2)
if p then expr1
else expr2

3) 
if p
then expr1
then expr2

how to model this formatting rule?

+ : alternatives
<0> space
<1> newline

if p <0> then expr1 <0> else expr2
+ if p <0> then expr1 <1> else expr2
+ if p <1> then expr1 <1> else expr2

if p <0> then expr1 (<0> + <1>) else expr2
+ if p <1> then expr1 <1> else expr2

using operators and functions

group(group(if p <1> then expr1) <1> else expr2)

group() is a chunk of inlineable elements
group!! makes alternatives

nesting!!

if p 
then if q
<-5->then expr1
<-5->else expr2
else expr3

nested block !!

nest i x


Document!!

type Doc = ???
type Layout = String

pretty :: Int -> Doc -> Layout
layouts :: Doc -> [Layout]

(<>) :: Doc -> Doc -> Doc
nil :: Doc
text :: String -> Doc
line :: Doc
nest :: Int -> Doc -> Doc
group :: Doc -> Doc
flatten :: Doc -> Doc



(x <> y) <> z = x <> (y <> z)
x <> nil = x
nil <> x = x

text (s ++ t) = text s <> text t
text "" = nil

nest i (x <> y) = nest i x <> nest i y
nest i nil = nil
nest i (text s) = text s
nest i line = line <> text (replicate i ' ')
nest i (nest j x) = nest (i+j) x
nest 0 x = x
nest i (group x) = group (nest i x)

layouts (x <> y) = layouts x <++> layouts y
layouts nil = [""]
layouts (text s) = [s]
layouts line = ["\n"]
layouts (nest i x) = map (nestl i) (layouts x)
layouts (group x) = layouts (flatten x) ++ layouts x


> prop1 = \(ArbDoc x) (ArbDoc y) -> flatten (x <> y) == flatten x <> flatten y

flatten nil = nil
flatten (text s) = text s
flatten line = text " "
flatten (nest i x) = flatten x
flatten (group x) = flatten x




example

text "Hello" <> line <> text "World!"
group (text "Hello" <> line <> text "World!")

> type Doc = [Layout]
> type Layout = String


(<>) :: Doc -> Doc -> Doc
nil :: Doc
text :: String -> Doc
line :: Doc
nest :: Int -> Doc -> Doc
group :: Doc -> Doc
flatten :: Doc -> Doc

> layouts = id
> x <> y = x <++> y
> nil = [""]
> text s = [s]
> line = ["\n"]
> nest i x = map (nestl i) x
> group x = flatten x ++ x
> flatten x = [flattenl (head x)]


> xs <++> ys = [x ++ y | x <- xs, y <- ys]
> nestl i x = concat (map f x)
>   where f c = if c == '\n' then '\n':replicate i ' ' else [c]
> flattenl [] = []
> flattenl (c:cs) = if c == '\n' then ' ' : flattenl (dropWhile (==' ') cs) else c : flattenl cs


> shape :: Layout -> [Int]
> shape = map length . lines


If conditional expression

> data CExpr = Expr String | If String CExpr CExpr
> cexpr :: CExpr -> Doc
> cexpr (Expr s) = text s
> cexpr (If p expr1 expr2) = group(group(text "if " <> text p <> line <> text "then " <> nest 5 (cexpr expr1))<> line <> text "else " <> nest 5 (cexpr expr2))


GenTree


Paragraphs

> para :: String -> Doc
> para = cvt . map text . words

> cvt [] = nil
> cvt (x:xs) = x <> foldr (<>) nil [group (line <> x) | x <- xs]



comparing two layouts
1) both fit in -> longer one wins
2) both don't fit -> shotrer one wins
3) otherwise fitting one wins

> pretty :: Int -> Doc -> Layout
> pretty w = fst . foldr1 choose . map augment
>   where
>    augment lx = (lx, shape lx)
>    choose alx aly
>     = if better (snd alx) (snd aly) then alx else aly
>    better [] ks = True
>    better js [] = False
>    better (j:js) (k:ks) | j == k = better js ks
>                         | otherwise = (j <= w)



> take5 :: [Char] -> [Char]
> take5 = take 5

> data ArbDoc = ArbDoc Doc deriving (Eq, Show)
> instance Arbitrary ArbDoc where
>   arbitrary = sized arbDoc
> arbDoc :: Int -> Gen ArbDoc
> arbDoc 0 = do
>   s <- listOf $ elements " ab"
>   elements $ map ArbDoc $ [nil, line, text s]
> arbDoc n = do
>   (Positive m) <- arbitrary
>   a <- arbDoc (n-1)
>   b <- arbDoc (n-1)
>   return  $ arbBranch a b (m `mod` 3)
> arbBranch :: ArbDoc -> ArbDoc -> Int -> ArbDoc
> arbBranch (ArbDoc a) (ArbDoc b) 0 = ArbDoc $ a <> b
> arbBranch (ArbDoc a) (ArbDoc b) 1 = ArbDoc $ nest 1 a
> arbBranch (ArbDoc a) (ArbDoc b) 2 = ArbDoc $ group a


> main :: IO ()
> main = do
>    putStrLn "Welcome to FP Haskell Center!"
>    putStrLn "Have a good day!"
>    print $ pretty 30 $ para "This is a fairly short paragraph with just twenty-two words. The problem is that pretty-printing it takes time, in fact 31.32 seconds"


    mapM_ putStrLn $ layouts $ cexpr $ If "p" (If "q" (Expr "expr1") (Expr "expr2")) (Expr "expr3")
    print $ map shape $ layouts $ cexpr $ If "p" (If "q" (Expr "expr1") (Expr "expr2")) (Expr "expr3")
    
quickCheck prop1
