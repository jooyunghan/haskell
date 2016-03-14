{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}

import Control.Monad
import Data.Monoid
import Data.Map (Map,empty,toList,insertWith)
import Data.List
import Control.Monad.State
import Control.Monad.Except
import Control.Exception (catch)

data Diagnostics s = Diagnostics {unD :: Map String Int} deriving Show
emptyD = Diagnostics empty

add :: String -> Diagnostics s -> Diagnostics s
add label d = d {unD = insertWith (+) label 1 $ unD d}

showDiagnostics d = "[" ++ (intercalate ", " . fmap f . toList . unD $ d) ++ "]"
                    where f (s,n) = s ++ "=" ++ show n

type Stack a = [a]
push = (:)
top = head
pop = tail
isEmpty = null

newtype StateMonadPlus s a =  SMP {runSMP :: StateT (Stack s) (StateT (Diagnostics s) (StateT s (Except String))) a}

diagnostics :: StateMonadPlus s String
diagnostics = SMP $ do
  diag "diag"
  showDiagnostics <$> (lift get)
  
annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate label m = SMP $ do
  diag label
  runSMP m

instance Functor (StateMonadPlus s) where
  fmap = liftM

instance Applicative (StateMonadPlus s) where
  pure = return
  (<*>) = ap

instance Monad (StateMonadPlus s) where
  return a = SMP $ do
    diag "return"
    return a
  m >>= f = SMP $ do
    diag "bind"
    a <- runSMP m
    runSMP (f a)

data Hole = Hole

instance MonadState s (StateMonadPlus s) where
  put s = SMP $ do
    diag "put"
    lift.lift.put $ s
  get = SMP $ do
    diag "get"
    lift.lift $ get

instance MonadError String (StateMonadPlus s) where
  throwError s = SMP $ do
    lift $ throwError s
  catchError m handler = SMP $ do
    catchError (runSMP m) (runSMP . handler)

e1 = do return 3 >> return 4
        return 5
        diagnostics

e2 = do annotate "A" (return 3 >> return 4)
        return 5
        diagnostics

e2' :: StateMonadPlus s a
e2' = throwError "hell"

runStateMonadPlus :: StateMonadPlus s a -> s -> Either String (a, s)
runStateMonadPlus m s = runExcept $ (runStateT (evalStateT (evalStateT (runSMP m) []) emptyD) s)

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

e3' :: (StoreState s m) => m()
e3' = get >> loadState

diag s = lift.modify $ add s

instance StoreState s (StateMonadPlus s) where
  saveState = SMP $ do
    diag "save"
    (s::s) <- lift.lift $ get
    modify (push s)
  loadState = SMP $ do
    diag "load"
    s <- get
    when (isEmpty s) (throwError "empty stack")
    lift.lift.put $ top s
    modify pop
