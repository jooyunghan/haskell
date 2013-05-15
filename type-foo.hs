data Barry t k p = Barry { yabba :: p, dabba :: t k} deriving (Show)

instance Functor (Barry t k) where
 fmap f (Barry x y) = Barry (f x) y


