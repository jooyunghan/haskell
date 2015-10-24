

data ListF a r = NilF | ConsF a r

instance Functor (ListF a) where
  fmap f NilF = NilF
  fmap f (ConsF x r) = ConsF x (f r)

