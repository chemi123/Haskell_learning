import Data.Monoid

--instance Functor ((,) a) where
--  fmap :: (b -> c) -> (a, b) -> (a, c)
--  fmap f (a, b) = (a, f b)


newtype Pair b a = Pair { getPair :: (a, b) } deriving (Show, Eq, Ord)


instance Functor (Pair c) where
  --fmap :: (a -> b) -> Pair c a -> Pair c b
  fmap f (Pair (x, y)) = Pair (f x, y)


instance Functor ((,,) a b) where
  --fmap :: (c -> d) -> (,,) a b c -> (,,) a b d
  fmap f (a, b, c) = (a, b, f c)


instance Functor ((,,,) a b c) where
  -- fmap :: (d -> e) -> (,,,) a b c d -> (,,,) a b c e
  fmap f (a, b, c, d) = (a, b, c, f d)


newtype Triple b c a = Triple { getTriple :: (a, b, c) } deriving (Show, Eq, Ord)


instance Functor (Triple c d) where
  -- fmap :: (a -> b) -> Triple c d a -> Triple c d b
  fmap f (Triple (a, b, c)) = Triple (f a, b, c)


--instance Monoid [a] where
--  mempty = []
--  mappend = (++)
