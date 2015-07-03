module Pretty (
    Doc,
    (<>), nil, line, text, pretty, shape, 
    nest, layouts, group
  ) where


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

flatten :: Doc -> Doc
flatten nil = nil
flatten (text s) = text s
flatten line = text ' '
flatten (x <> y) = flatten x <> flatten y
flatten (nest i x) = flatten x
flatten (group x) = flatten x


A <> (B <> C)

-}




type Doc = [Layout]
type Layout = String

pretty :: Int -> Doc -> String
pretty w x = fst $ foldr1 choose $ augment x
  where
    augment x = [ (l, shape l) | l <- x ]
    choose lx ly = if better (snd lx) (snd ly) then lx else ly
    better sx [] = True
    better [] sy = False
    better (x:sx) (y:sy) = if x == y
        then better sx sy
        else if x < w then True
             else False

layouts :: Doc -> [Layout]
layouts = id

nil :: Doc
nil = [""]

line :: Doc
line = ["\n"]

text :: String -> Doc
text s = [s]

(<>) :: Doc -> Doc -> Doc
x <> y =  x <++>  y

nest :: Int -> Doc -> Doc
nest i =  map (nestl i) 

group :: Doc -> Doc
group x =  flatten x  ++ x 

flatten :: Doc -> Doc
flatten x =  [flattenl (head x)]

---

xs <++> ys = [x++y | x<-xs, y<-ys]

nestl i xs = concat [p x | x<-xs]
 where p x = if x == '\n' then '\n':replicate i ' ' else [x]

flattenl [] = []
flattenl (x:xs) = if x == '\n' then ' ' : flattenl (dropWhile (==' ') xs) else x : flattenl xs

---

shape :: Layout -> [Int]
shape = map length . lines
