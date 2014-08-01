module Minimax (aimove) where

import Tree


gametree :: (a -> [a]) -> a -> Tree a
gametree = reptree 

maximize :: (Ord a) => Tree a -> a
maximize (Node n []) = n
maximize (Node n sub) = foldl1 max (map minimize sub)

minimize :: (Ord a) => Tree a -> a
minimize (Node n []) = n
minimize (Node n sub) = foldl1 min (map maximize sub)

-- dynamic evaluator
evaluate :: (a -> [a]) -> (a -> Int) -> a ->  Int
evaluate moves static = maximize . maptree static . gametree moves

aimove :: (a -> [a]) -> (a -> Int) -> a -> a
aimove moves static p = maxkey [(evaluate' move, move)| move <- moves p]
	where
		evaluate' = minimize . maptree static . prune 5 . gametree moves
		maxkey = snd . foldl1 (\(k1,v1) (k2,v2) -> if k1 > k2 then (k1,v1) else (k2,v2))

prune :: Int -> Tree a -> Tree a
prune 0 (Node a _) = Node a []
prune n (Node a sub) = Node a (map (prune (n-1)) sub)
