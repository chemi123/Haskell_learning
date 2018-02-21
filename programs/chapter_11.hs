import Control.Applicative

fmapIO :: (a -> b) -> IO a -> IO b
fmapIO f action = do
  result <- action
  return (f result)


--instance Functor ((->) r) where
--  fmap :: (a -> b) -> ((->) r a) -> ((->) r b)
--  (Functor f) -> (a -> b) -> f a -> f b

--instance Applicative Maybe where
--  pure Nothing = Nothing
--  pure = Just
--  Nothing <*> _ = Nothing
--  (Just f) <*> something = fmap f something
--
--
--instance Applicative [] where
--  pure x = [x]
--  fs <*> xs = [f x | f <- fs, x <- xs]



--instance Applicative IO where
--  pure = return
--  a <*> b = do
--    f <- a
--    x <- b
--    return (f x)


-- instance Applicative ZipList where
--   pure x = ZipList (repeat x)
--   ZipList fs <*> ZipList xs = ZipList (zipWith (\f x -> f x) fs xs)


--fmapFunction :: (a -> b) -> ((->) r a) -> ((->) r b)
fmapFunction :: (a -> b) -> (r -> a) -> (r -> b)
fmapFunction f g = \x -> f (g x)


fmapFunction' :: (a -> b) -> (r -> a) -> (r -> b)
fmapFunction' = (.)


fmapIO' :: (a -> b) -> IO a -> IO b
fmapIO' f action = do
  result <- action
  return (f result)


fmapList :: (a -> b) -> [a] -> [b]
fmapList f [] = []
fmapList f (x:xs) = f x : fmapList f xs


fmapMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapMaybe f Nothing = Nothing
fmapMaybe f (Just x) = Just (f x)


factorialMaybe :: Int -> Maybe Int
factorialMaybe n
  | n < 0 = Nothing
  | n == 0 = Just 1
  | otherwise = (*) <$> Just n <*> factorialMaybe (n-1)
