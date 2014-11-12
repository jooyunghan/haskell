import Data.List
--               1         2         3         4   
--     01234567890123456789012345678901234567890123456789
ants = iterate(concatMap(sequence[head,length]).group)[1]
