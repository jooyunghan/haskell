module Tree (Tree(Node), reptree, maptree) where

data Tree n = Node n [Tree n]

reptree :: (a -> [a]) -> a -> Tree a
reptree f a = Node a (map (reptree f) (f a))

maptree :: (a -> b) -> Tree a -> Tree b
maptree f (Node a subs) = Node (f a) (map (maptree f) subs)
