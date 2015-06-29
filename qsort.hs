import Data.List

qsort [] = []
qsort (x:xs) = intercalate[x](map(qsort)(sequence(map filter[(<=x),(>x)])xs))
