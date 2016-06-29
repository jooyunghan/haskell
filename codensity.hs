{-# LANGUAGE RankNTypes, MultiParamTypeClasses, FunctionalDependencies, FlexibleContexts, FlexibleInstances, UndecidableInstances, DeriveFunctor #-}

import Criterion.Main
import Prelude hiding (abs)
import Control.Monad

-- Free --

data Free f a = Pure a | Wrap (f (Free f a))

instance (Show (f (Free f a)), Show a) => Show (Free f a) where
  showsPrec d (Pure a) = showParen (d > 10) $
    showString "Pure " . showsPrec 11 a
  showsPrec d (Wrap m) = showParen (d > 10) $
    showString "Wrap " . showsPrec 11 m

instance (Functor f) => Functor (Free f) where
  fmap = liftM

instance (Functor f) => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance (Functor f) => Monad (Free f) where
  (Pure a) >>= f = f a
  (Wrap x) >>= f = Wrap (fmap (>>= f) x)

class (Functor f, Monad m) => FreeLike f m | m -> f where
   wrap :: f (m a) -> m a

instance (Functor f) => FreeLike f (Free f) where
  wrap = Wrap

-- Codensity --

newtype Codensity m a = Codensity {runCodensity :: forall b. (a -> m b) -> m b}

instance (Monad f) => Functor (Codensity f) where
  fmap = liftM

instance (Monad f) => Applicative (Codensity f) where
  pure a = Codensity (\h -> h a)
  (<*>) = ap

instance (Monad f) => Monad (Codensity f) where
  m >>= f = Codensity (\k -> runCodensity m (\a -> runCodensity (f a) k))

instance (Functor f, FreeLike f m) => FreeLike f (Codensity m) where
  wrap x = Codensity (\h -> wrap (fmap (\p -> runCodensity p h) x))

instance Show (Codensity m a) where
  showsPrec d (Codensity _) = showParen (d > 10) $
    showString "Codensity ..."

abs :: Monad m => Codensity m a -> m a
abs m = runCodensity m return

improve :: Functor f => (forall m. FreeLike f m => m a) -> Free f a
improve m = abs m

-- Binary tree example --

data F a = N a a deriving (Show, Functor)
type T = Free F

leaf :: Monad m => a -> m a
leaf = return

node :: FreeLike F m => m a -> m a -> m a
node a b = wrap (N a b)

fullTree :: (FreeLike F m) => Int -> m Int
fullTree 0 = leaf 0
fullTree n = do
  i <- fullTree (n-1)
  node (leaf (n-i)) (leaf i)

zigzag :: T a -> a
zigzag = zig
  where zig (Pure a) = a
        zig (Wrap (N a b)) = zag a
        zag (Pure a) = a
        zag (Wrap (N a b)) = zig b

main :: IO ()
main = print $ zigzag $ improve $ fullTree 100
