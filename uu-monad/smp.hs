{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Monad
import Data.Monoid
import Data.Map
import Data.List
import Control.Monad.State

type Stack a = [a]
push :: a -> Stack a -> Stack a
push = (:)
top = head
pop = tail

data Diagnostics s = Diagnostics {unD :: Map String Int, stack :: Stack s} deriving Show

emptyD = Diagnostics empty []

add :: String -> Diagnostics s -> Diagnostics s
add label d = d {unD = insertWith (+) label 1 $ unD d}
addBind = add "bind"
addReturn = add "return"
addDiagnostics = add "diagnostics"

showDiagnostics d = "[" ++ (intercalate ", " . fmap f . toList . unD $ d) ++ "]"
                    where f (s,n) = s ++ "=" ++ show n

data StateMonadPlus s a = SMP {runSMP :: (Diagnostics s, s) -> (Diagnostics s, Either String (a,s))}

diagnostics :: StateMonadPlus s String
diagnostics = SMP $ \(d,s) -> let d' = addDiagnostics d
                              in (d', Right (showDiagnostics d', s))

annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate label m = SMP $ \(d,s) -> runSMP m (add label d, s)

instance Functor (StateMonadPlus s) where
  fmap = liftM

instance Applicative (StateMonadPlus s) where
  pure = return
  (<*>) = ap

instance Monad (StateMonadPlus s) where
  return a = SMP $ \(d, s) -> (addReturn d,  Right (a, s))
  m >>= f = SMP $ \(d, s) -> let (d', result) = runSMP m (addBind d, s)
                             in case result of
                             Left err -> (d', Left err)
                             Right (a, s') -> runSMP (f a) (d', s')
  fail msg = SMP $ \(d, _) -> (add "fail" d, Left msg)

instance MonadState s (StateMonadPlus s) where
  get = SMP $ \(d,s) -> (add "get" d, Right (s, s))
  put s = SMP $ \(d,_) -> (add "put" d, Right ((), s))

e1 = do return 3 >> return 4
        return 5
        diagnostics

e2 = do annotate "A" (return 3 >> return 4)
        return 5
        diagnostics

runStateMonadPlus :: StateMonadPlus s a -> s -> Either String (a, s)
runStateMonadPlus m s = snd $ runSMP m (emptyD, s)


class MonadState s m => StoreState s m|m->s where
  saveState :: m ()
  loadState :: m ()

e3 :: (Num s, StoreState s m) => m (s,s,s,s,s)
e3 = do i1<-get;saveState
        modify (*2)
        i2<-get;saveState
        modify (*2)
        i3<-get;loadState
        i4<-get;loadState
        i5<-get
        return (i1,i2,i3,i4,i5)

instance StoreState s (StateMonadPlus s) where
  saveState = SMP $ \(d,s) -> (d {stack = push s (stack d)}, Right((),s))
  loadState = SMP $ \(d,s) -> (d {stack = pop (stack d)}, Right((), top (stack d)))
