module Pretty
	(Doc, Layout, 
	 nil, line, text,
	 nest, (<>), group,
	 layouts, pretty, layout) where

{- problem is pretty printing.

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




prop1 = \(ArbDoc x) (ArbDoc y) -> flatten (x <> y) == flatten x <> flatten y

flatten nil = nil
flatten (text s) = text s
flatten line = text " "
flatten (nest i x) = flatten x
flatten (group x) = flatten x




example

text "Hello" <> line <> text "World!"
group (text "Hello" <> line <> text "World!")
-}

--type Doc = [Layout]
data Doc = Nil
		 | Line
		 | Text String
		 | Nest Int Doc
		 | Group Doc
		 | Doc :<>: Doc
		 deriving (Eq, Show)

type Layout = String

{-
(<>) :: Doc -> Doc -> Doc
nil :: Doc
text :: String -> Doc
line :: Doc
nest :: Int -> Doc -> Doc
group :: Doc -> Doc
flatten :: Doc -> Doc
-}

{-
layouts (x <> y) = layouts x <++> layouts y
layouts nil = [""]
layouts (text s) = [s]
layouts line = ["\n"]
layouts (nest i x) = map (nestl i) (layouts x)
layouts (group x) = layouts (flatten x) ++ layouts x
-}
--layouts :: Doc -> [Layout]
--layouts Nil = [""]
--layouts Line = ["\n"]
--layouts (Text s) = [s]
--layouts (x :<>: y) = layouts x <++> layouts y
--layouts (Nest i x) = map (nestl i) $ layouts x
--layouts (Group x) = layouts (flatten x) ++ layouts x


x <> y = x :<>: y
nil = Nil
text s = Text s
line = Line
nest i x = Nest i x
group x = Group x

{-
flatten (x <> y) = flatten x <> flatten y
flatten nil = nil
flatten (text s) = text s
flatten line = text " "
flatten (nest i x) = flatten x
flatten (group x) = flatten x
-}
flatten :: Doc -> Doc
flatten Nil = Nil
flatten (Text s) = Text s
flatten Line = Text " "
flatten (x :<>: y) = flatten x :<>: flatten y
flatten (Nest i x) = flatten x
flatten (Group x) = flatten x


xs <++> ys = [x ++ y | x <- xs, y <- ys]
nestl i x = concat (map f x)
  where f c = if c == '\n' then '\n':replicate i ' ' else [c]
flattenl [] = []
flattenl (c:cs) = if c == '\n' then ' ' : flattenl (dropWhile (==' ') cs) else c : flattenl cs


-- layr = layouts . toDoc
toDoc :: [(Int, Doc)] -> Doc
toDoc ids = foldr (:<>:) Nil [Nest i x| (i,x) <- ids]

layr :: [(Int, Doc)] -> [Layout]

layouts :: Doc -> [Layout]
layouts x = layr [(0, x)]

layr [] = [""]
layr ((i, x:<>:y):ids) = layr ((i,x):(i,y):ids)
layr ((i, Nil):ids) = layr ids
layr ((i, Line):ids) = [ '\n':replicate i ' '++ ls|ls <- layr ids] 
layr ((i, (Text s)):ids) = [s ++ ls | ls <- layr ids]
layr ((i, (Nest j x)):ids) = layr ((i+j, x):ids)
layr ((i, (Group x)):ids) = layr ((i,flatten x):ids) ++ layr ((i,x):ids)


{-
comparing two layouts
1) both fit in -> longer one wins
2) both don't fit -> shotrer one wins
3) otherwise fitting one wins
-}

--pretty :: Int -> Doc -> Layout
--pretty w = fst . foldr1 choose . map augment
--  where
--   augment lx = (lx, shape lx)
--   choose alx aly
--     = if better (snd alx) (snd aly) then alx else aly
--   better [] ks = True
--   better js [] = False
--   better (j:js) (k:ks) | j == k = better js ks
--                        | otherwise = (j <= w)

pretty :: Int -> Doc -> Layout
pretty w x = best w [(0, x)]
 where
 	best r [] = ""
 	best r ((i,x:<>:y):ids) = best r ((i,x):(i,y):ids)
 	best r ((i,Nil):ids) = best r ids
 	best r ((i,Line):ids) = '\n':replicate i ' ' ++ best (w-i) ids
 	best r ((i,(Text s)):ids) = s ++ best (r - length s) ids
 	best r ((i,(Nest j x)):ids) = best r ((i+j,x):ids)
 	best r ((i,(Group x)):ids) = better r
 									(best r ((i,flatten x):ids))
 									(best r ((i,x):ids))
 	better r lx ly = if fits r lx then lx else ly
 	fits r _ | r < 0 = False
 	fits r [] = True
 	fits r (c:cs) = if c == '\n' then True else fits (r-1) cs


layout :: Layout -> IO ()
layout = putStrLn


