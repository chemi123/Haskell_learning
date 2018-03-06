import Data.Monoid
import Control.Monad.Writer
import Control.Monad.Reader

isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")


applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)


applyMonoid :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyMonoid (x, monoid) f = let (y, newMonoid) = f x in (y, monoid `mappend` newMonoid)


type Food = String
type Price = Sum Int


addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whisky", Sum 99)
addDrink _ = ("beer", Sum 30) 

--Writer w a = Writer { runWriter :: (a, w) }
--
--instance (Monoid w) => Monad (Writer w) where
--  return x = Writer (x, mempty)
--  Writer (x, w) >>= f = let (Writer (y, w')) = f x
--                        in Writer (y, w `mappend` w')

productWriter :: Writer (Product Int) Int
productWriter = do
  x <- (return 2 :: Writer (Product Int) Int)
  y <- writer (2, Product x)
  tell $ Product 2
  z <- writer (3, Product y)
  writer (2, Product z)


gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
  | b == 0 = do
      tell ["Finished with " ++ show a]
      return a
  | otherwise = do
      tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]
      gcd' b (a `mod` b)


--instance Applicative ((->) r) where
--  pure :: (Applicative f) => a -> f a
--  pure :: a -> (->) r a
--  pure :: a -> (r -> a)
--
--  (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b
--  (<*>) :: (->) r (a -> b) -> (->) r a -> (->) r b
--  (<*>) :: (r -> (a -> b)) -> (r -> a) -> (r -> b)
--  f <*> g = \x -> f x (g x)
--
--
--instance Monad ((->) r) where
--  return :: (Monad m) => a -> m a
--  return :: a -> (->) r a
--  return :: a -> (r -> a)
--  return x = \_ -> x
--
--  (>>=) = (Monad m) => m a -> (a -> m b) -> m b
--  (>>=) = (->) r a -> (a -> (->) r b) -> (->) r b
--  (>>=) = (r -> a) -> (a -> (r -> b)) -> (r -> b)
--  h >>= f = \w -> f (h w) w
