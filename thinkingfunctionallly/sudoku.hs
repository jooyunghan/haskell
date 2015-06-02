import Data.List (transpose, nub)

easy =  ["79....3..",
		".....69..",
		"8...3..76",
		".....5..2",
		"..54187..",
		"4..7.....",
		"61..9...8",
		"..23.....",
		"..9....54"]

solution =  ["796854321",
			"243176985",
			"851239476",
			"137965842",
			"925418763",
			"468723519",
			"614597238",
			"582341697",
			"379682154"]

class Backtrackable m where
	valid :: m -> Bool
	completed :: m -> Bool
	next :: m -> [m]

solve :: (Backtrackable m) => m -> [m]
solve m
	| not (valid m) = []                -- has dup
	| completed m = [m]                 -- no dup, no blank => this is a solution
	| otherwise   = next m >>= solve    -- no dup, has blank => keep searching

type Digit = Char
type Matrix a = [[a]]

newtype Sudoku = Sudoku (Matrix Digit) deriving (Eq, Show)

instance Backtrackable Sudoku where
	valid (Sudoku m) = all ok (rows m ++ cols m ++ boxes m)
	completed (Sudoku m) = all (not . blank) (concat m)
	next (Sudoku m) = map Sudoku [rows0 ++  (cells0 ++ d:cells1):rows1| d <- ['1'..'9']]
		where
			(rows0, row:rows1) = break (any blank) m
			(cells0, cell:cells1) = break blank row

ok = nodups . filter (not . blank)
nodups cells = length cells == length (nub cells)
blank = (== '.')

rows = id
cols = transpose
boxes = map ungroup . ungroup . map transpose . group . map group

ungroup = concat
group [] = []
group xs = take 3 xs : group (drop 3 xs)

main = print $ solve (Sudoku easy)