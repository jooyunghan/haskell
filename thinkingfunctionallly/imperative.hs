


sequence_ :: (Monad m) => [m a] -> m ()
sequence_ = foldr (>>) done

sequence :: (Monad m) => [m a] -> m [a]
sequence = foldr k (return [])
            where k a p = do {x <- a; y <- p; return (x:y)}

done :: (Monad m) -> m ()
done = return ()

forM_ :: (Monad m) => [a] -> (a -> m b) -> m ()
forM_ = flip mapM_

mapM_ :: (Monad m) => (a -> m b) -> [a] -> m ()
mapM_ f = sequence_ . map f

mapM :: (Monad m) => (a -> m b) -> [a] -> m [a]
mapM f = sequence . map f
