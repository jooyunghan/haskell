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


moves :: Position -> [Position]
moves p =  M.elems $ uniqMap normalValue $ fill p
	where uniqMap keyFun list = M.fromListWith (\a -> id) [(keyFun v, v) | v <- list]

gametree :: Position -> Tree Position
gametree = reptree moves


-- Computer is Naught
-- static evaluates the current position as 1 for winning, -1 for losing, 0 otherwise
static :: Position -> Int
static p = case winner p of
	Cross -> -1
	Naught -> 1
	otherwise -> 0

maximize (Node n []) = n
maximize (Node n sub) = foldl1 max (map minimize sub)
minimize (Node n []) = n
minimize (Node n sub) = foldl1 min (map maximize sub)

-- dynamic evaluator
evaluate :: Position -> Int
evaluate = maximize . maptree static . gametree

aimove :: Position -> Position
aimove p = maxkey [(evaluate' move, move)| move <- moves p]
	where
		evaluate' = minimize . maptree static . gametree
		maxkey = snd . foldl1 (\(k1,v1) (k2,v2) -> if k1 > k2 then (k1,v1) else (k2,v2))

winner :: Position -> Cell
winner (a,b,c,d,e,f,g,h,i) 
	| a == b && b == c && a /= Empty = a
	| d == e && e == f && d /= Empty = d 
	| g == h && h == i && g /= Empty = g
	| a == d && d == g && a /= Empty = a
	| b == e && e == h && b /= Empty = b
	| c == f && f == i && c /= Empty = c
	| a == e && e == i && a /= Empty = a
	| c == e && e == g && c /= Empty = c
	| otherwise = Empty

end :: Position -> Bool
end p = winner p /= Empty || count p Empty == 0

data Tree n = Node n [Tree n]

reptree :: (a -> [a]) -> a -> Tree a
reptree f a = Node a (map (reptree f) (f a))

maptree :: (a -> b) -> Tree a -> Tree b
maptree f (Node a subs) = Node (f a) (map (maptree f) subs)

player :: Position -> IO (Position)
player p = do
	putStrLn $ render p
	putStrLn "Your move(0~8):"
	input <- getLine
	return $ update p (read input) Cross

loop :: Position -> IO ()
loop p = do
	m <- player p
	putStrLn $ render m
	if winner m == Cross 
		then putStrLn "You win" 
		else if end m 
			then putStrLn "Draw"
			else 
				let m2 = aimove m in if winner m == Naught 
					then putStrLn "I win"
					else if end m2
						then putStrLn "Draw"
						else loop m2

		
main = loop start