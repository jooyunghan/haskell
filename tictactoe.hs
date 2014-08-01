import qualified Data.Map as  M

data Cell = Cross | Naught | Empty deriving (Eq, Show)

to_char Cross = 'X'
to_char Naught = 'O'
to_char Empty = '.'

to_int Cross = 2
to_int Naught = 1
to_int Empty = 0

type Position = (Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell, Cell)

start = (Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty)

render :: Position -> String
render (a,b,c,d,e,f,g,h,i) = unlines [map to_char row | row <- [[a,b,c],[d,e,f],[g,h,i]]]

to_list :: Position -> [Cell]
to_list (a,b,c,d,e,f,g,h,i) = a:b:c:d:e:f:g:h:i:[]

moves :: Position -> [Position]
moves p =  M.elems $ uniqMap normalValue $ fill p
	where uniqMap keyFun list = M.fromListWith (\a -> id) [(keyFun v, v) | v <- list]

-- find minimum value among 8 variations of current Position
normalValue :: Position -> Int
normalValue p = foldl1 min [ value (f (g p)) | f<-[id, flip_horizontal], g<-[id, rot90, rot90 . rot90, rot90 . rot90 . rot90]]

-- convert Position into value by assuming the Position as digits of radix-3
value :: Position -> Int
value p = foldl (\a b-> a * 3 + b) 0 (map to_int (to_list p))

flip_horizontal :: Position -> Position
flip_horizontal (a,b,c,d,e,f,g,h,i) = (c,b,a,f,e,d,i,h,g)

rot90 :: Position -> Position
rot90 (a,b,c,d,e,f,g,h,i) = (g,d,a,h,e,b,i,f,c)

fill :: Position -> [Position]
fill p = fill_next p (next_turn p)

fill_next :: Position -> Cell -> [Position]
fill_next p c = [update p i c| i <- [0..8], cell p i == Empty]

cell p i = (to_list p) !! i

update :: Position -> Int -> Cell -> Position
update (a,b,c,d,e,f,g,h,i) 0 cell = (cell,b,c,d,e,f,g,h,i)
update (a,b,c,d,e,f,g,h,i) 1 cell = (a,cell,c,d,e,f,g,h,i)
update (a,b,c,d,e,f,g,h,i) 2 cell = (a,b,cell,d,e,f,g,h,i)
update (a,b,c,d,e,f,g,h,i) 3 cell = (a,b,c,cell,e,f,g,h,i)
update (a,b,c,d,e,f,g,h,i) 4 cell = (a,b,c,d,cell,f,g,h,i)
update (a,b,c,d,e,f,g,h,i) 5 cell = (a,b,c,d,e,cell,g,h,i)
update (a,b,c,d,e,f,g,h,i) 6 cell = (a,b,c,d,e,f,cell,h,i)
update (a,b,c,d,e,f,g,h,i) 7 cell = (a,b,c,d,e,f,g,cell,i)
update (a,b,c,d,e,f,g,h,i) 8 cell = (a,b,c,d,e,f,g,h,cell)

-- assume Cross first
next_turn :: Position -> Cell
next_turn p = if count p Cross == count p Naught then Cross else Naught

count :: Position -> Cell -> Int
count p c = length $ filter (== c) $ to_list p

main = do
	putStrLn . unlines . map render . moves . last . moves $ start
	putStrLn "Hello World"