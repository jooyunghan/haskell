import Data.Map
import Prelude hiding (lookup)

data Expr v = Var v | Val Int | Add (Expr v) (Expr v) deriving (Show, Eq)

type Env v = Map v Int

-- eval :: (Ord v) =>  Expr v -> Env v -> Int
-- eval (Var v) env = env ! v
-- eval (Val i) env = i
-- eval (Add l r) env = (eval l env) + (eval r env)

eval :: (Ord v) => Expr v -> Env v -> Int
eval (Var v) = flip (!) v
eval (Val i) = const i
eval (Add l r) = const (+) `s` eval l `s` eval r

s :: (c -> a -> b) -> (c -> a) -> c -> b
s f a c = (f c) (a c)

ex1 :: Expr String
ex1 = Add (Val 3) (Var "x")

env1 :: Env String
env1 = singleton "x" 5

