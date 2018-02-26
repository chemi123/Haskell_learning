applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing `applyMaybe` f = Nothing
(Just x) `applyMaybe` f = f x

--instance Monad Maybe where
--  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--  Nothing >>= f = Nothing
--  (Just x) >>= f = f x


type Birds = Int
type Pole = (Birds, Birds)


landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing


landRight:: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - abs (right + n)) < 4 = Just (left, right + n)
  | otherwise                        = Nothing
