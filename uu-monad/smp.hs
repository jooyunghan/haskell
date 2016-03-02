import Control.Monad
import Data.Monoid
import Data.Map
import Data.List

newtype Diagnostics = Diagnostics {unD :: Map String Int} deriving Show

instance Monoid Diagnostics where
  mempty = Diagnostics (fromList []) 
  mappend d1 d2 = Diagnostics $ unD d1 <> unD d2

add label d = Diagnostics $ insertWith (+) label 1 $ unD d
addBind = add "bind"
addReturn = add "return"
addDiagnostics = add "diagnostics"

showDiagnostics d = "[" ++ (intercalate ", " . fmap f . toList . unD $ d) ++ "]"
                    where f (s,n) = s ++ "=" ++ show n

data StateMonadPlus s a = SMP {runSMP :: (Diagnostics,s) -> (a,Diagnostics,s)}

diagnostics :: StateMonadPlus s String
diagnostics = SMP $ \(d,s) -> let d' = addDiagnostics d
                              in (showDiagnostics d', d', s)

annotate :: String -> StateMonadPlus s a -> StateMonadPlus s a
annotate label m = SMP $ \(d,s) -> runSMP m (add label d, s)

instance Functor (StateMonadPlus s) where
  fmap = liftM

instance Applicative (StateMonadPlus s) where
  pure = return
  (<*>) = ap

instance Monad (StateMonadPlus s) where
  return a = SMP $ \(d, s) -> (a, addReturn d, s)
  m >>= f = SMP $ \(d, s) -> let (a, d', s') = runSMP m (addBind d, s)
                             in runSMP (f a) (d', s')  

e1 = do return 3 >> return 4
        return 5
        diagnostics

e2 = do annotate "A" (return 3 >> return 4)
        return 5
        diagnostics
