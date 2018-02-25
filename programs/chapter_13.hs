applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing `applyMaybe` f = Nothing
(Just x) `applyMaybe` f = f x

--instance Monad Maybe where
--  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--  Nothing >>= f = Nothing
--  (Just x) >>= f = f x
