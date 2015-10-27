


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
layouts = undefined

nil :: Doc
nil = undefined

line :: Doc
line = undefined

text :: String -> Doc
text = undefined

(<>) :: Doc -> Doc -> Doc
(<>) = undefined

nest :: Int -> Doc -> Doc
nest = undefined

group :: Doc -> Doc
group = undefined

flatten :: Doc -> Doc
flatten = undefined




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
<<<<<<< Updated upstream
=======
--    print $ map shape $ layouts $ cexpr c1
>>>>>>> Stashed changes
  putStrLn $ pretty 30 $ para pg

