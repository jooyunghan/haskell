

data Matrix a = Matrix {nrows ::Int,  ncols ::Int, elements ::[a]} 
    deriving (Show, Eq)

rows :: Matrix a -> [[a]]
rows mat = take (nrows mat) $ map (row mat) [0..]

cols :: Matrix a -> [[a]]
cols mat = take (ncols mat) $ map (col mat) [0..]

row mat r = take (ncols mat) [get_elem mat r c| c <- [0..]]
col mat c = take (nrows mat) [get_elem mat r c| r <- [0..]]

get_elem mat r c = (elements mat) !! (r * (ncols mat) + c)

update_mat mat f = Matrix (nrows mat) (ncols mat) new_elements
    where new_elements = [f r c n|(i,n) <- [0..] `zip` elements mat,
                                  let r = i `div` (ncols mat),
                                  let c = i `mod` (ncols mat)] 

set_zeros :: (Num a, Eq a) => Matrix a -> Matrix a
set_zeros mat = let row_zeros = map (elem 0) (rows mat)
                    col_zeros = map (elem 0) (cols mat)
                in update_mat mat (\r c n -> if row_zeros !! r || col_zeros !! c
                                             then 0
                                             else n)

                    
