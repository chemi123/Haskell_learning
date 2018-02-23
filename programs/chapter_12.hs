import Data.Monoid
import qualified Data.Foldable as F

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


data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving Show

instance Functor Tree where
  fmap f EmptyNode = EmptyNode
  fmap f (Node x left right) = Node (f x) (fmap f left) (fmap f right)

instance Foldable Tree where
  --foldMap :: (Monoid m) => (a -> m) -> Tree a -> m
  foldMap f EmptyNode = mempty
  foldMap f (Node x left right) = foldMap f left `mappend`
                                  f x            `mappend`
                                  foldMap f right

--instance Monoid [a] where
--  mempty = []
--  mappend = (++)


--newtype Product a = Product {getProduct :: a } deriving (Eq, Ord, Show, Read, Bounded)
--
--instance Monoid (Product a) where
--  mempty = Product 1
--  mappend :: Product a -> Product a -> Product a
--  Product x `mappend` Product y = Product (x * y)
--
--
--newtype Sum a = Sum {getSum:: a } deriving (Eq, Ord, Show, Read, Bounded)
--
--instance Monoid (Sum a) where
--  mempty = Sum 0
--  mappend :: Sum a -> Sum a -> Sum a
--  Sum x `mappend` Sum y = Sum (x + y)


--newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
--
--instance Monoid Any where
--  mempty = Any False
--  --mappend :: Any -> Any -> Any
--  Any x `mappend` Any y = Any (x || y)
--
--
--newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)
--instance Monoid All where
--  mempty = All True
--  --mappend :: All -> All -> All
--  All x `mappend` All y = All (x && y)

--instance Monoid Ordering where
--  LT `mappend` _ = LT
--  EQ `mappend` y = y
--  GT `mappend` _ = GT

--instance (Monoid a) => Monoid (Maybe a) where
--  mempty = Nothing
--  Nothing `mappend` m = m
--  m `mappend` Nothing = m
--  (Just m1) `mappend` (Just m2) = Just (m1 `mappend` m2)
