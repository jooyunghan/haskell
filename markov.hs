import Data.Map
import System.Random

group assoc = fromListWith (++) [(k,[v]) | (k,v) <- assoc]

mapChooseRandom g = zipWith chooseRandom (randoms g)
chooseRandom r list = list !! (r `mod` (length list))

learn str = group $ zip prefices suffices
    where prefices = zip w (tail w)
          suffices = tail (tail w)
          w = ["",""] ++ (words str) ++ [""]

generate g prefixMap = takeWhile (/= "") suffices
    where suffices = mapChooseRandom g $ fmap (prefixMap !) prefices
          w = "" : "" : suffices   
          prefices = zip w (tail w)  

main = do
    contents <- getContents
    g <- getStdGen
    putStrLn $ unwords $ take 100 $ generate g $ learn contents

