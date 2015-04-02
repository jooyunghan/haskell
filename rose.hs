import Prelude hiding (foldr)
import Data.Foldable
import Data.Monoid

data Rose a = a :> [Rose a] deriving (Eq, Show)

instance Foldable Rose where
	foldMap f (x :> xs) = f x <> foldMap (foldMap f) xs

t = 1 :> [2 :> [], 3 :> []]

newtype IntSum = IntSum Int

toInt :: IntSum -> Int
toInt (IntSum a) = a

instance Monoid IntSum where
	mempty = IntSum 0
	(IntSum a) `mappend` (IntSum b) = IntSum (a + b)

count :: Rose a -> Int
count a = toInt (foldMap (const (IntSum 1)) a)

countLeaves a = toInt (foldMap    a)

main = do
	print $ count t
	print $ countLeaves t