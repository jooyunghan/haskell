-- combi_sum 8 [2,1,10,7,6,5,1] 

import Data.List

combi_sum target = filter ((==target) . sum) . combis 

combis = combis_ . group . sort 
  where combis_ [] = [[]]
        combis_ (g:rest) = [p ++ s| p <- tails g, s <- combis_ rest]
--        combis_ (g:rest) = liftM2 (++) (tails g) (combis_ rest)


