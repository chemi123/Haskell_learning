import Control.Monad

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing `applyMaybe` f = Nothing
(Just x) `applyMaybe` f = f x

--instance Monad Maybe where
--  (>>=) :: Maybe a -> (a -> Maybe b) -> Maybe b
--  Nothing >>= f = Nothing --  (Just x) >>= f = f x

--instance Monad [] where
--  return x = [x]
--  (>>=) :: [a] -> (a -> [b]) -> [b]
--  xs >> f = concat [map f xs]

type Birds = Int
type Pole = (Birds, Birds)

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
  | abs ((left + n) - right) < 4 = Just (left + n, right)
  | otherwise                    = Nothing


landRight:: Birds -> Pole -> Maybe Pole
landRight n (left, right)
  | abs (left - (right + n)) < 4 = Just (left, right + n)
  | otherwise                        = Nothing


banana :: Pole -> Maybe Pole
banana _ = Nothing

foo :: Maybe String
foo = do
  x <- Just 3
  y <- Just "!"
  Just (show x ++ y)


pierrSucceed :: Maybe Pole
pierrSucceed = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 3 first
  third <- landLeft (-1) second
  landRight 1 third


pierrFail :: Maybe Pole
pierrFail = do
  start <- return (0, 0)
  first <- landLeft 2 start
  second <- landRight 3 first
  Nothing
  third <- landLeft (-1) second
  landRight 1 third


justH :: Maybe Char
justH = do
  (x:xs) <- Just "Hello!"
  return x


failPatternMatch :: Maybe Char
failPatternMatch = do
  (x:xs) <- Just ""
  return x


listOfTuples :: [(Int, Char)]
listOfTuples = do
  n <- [1, 2]
  ch <- ['a', 'b']
  return (n, ch)


sevensOnly :: [Int]
sevensOnly = do
  x <- [1..50]
  guard ('7' `elem` show x)
  return x


evenGuard :: [Int]
evenGuard = do
  x <- [1..10]
  guard (x `mod` 2 == 0)
  return x


type KnightPos = (Int, Int)

-- 引数が(1, 1)の場合は以下のように表現できる
-- return (1,1) >>= \(c, r)-> [(c+1, r+2), (c+2, r+1), (c-1, r+2), (c-2, r+1),
--                             (c+1, r-2), (c+2, r-1), (c-1, r-2), (c-2, r-1)]
--              >>= \(c', r') -> guard (c' `elem` [1..8] && r' `elem` [1..8])
--              >> return (c', r')
moveKnight :: KnightPos -> [KnightPos]
moveKnight (c, r) = do
  (c', r') <- [(c+1, r+2), (c+2, r+1), (c-1, r+2), (c-2, r+1),
               (c+1, r-2), (c+2, r-1), (c-1, r-2), (c-2, r-1)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')


in3 :: KnightPos -> [KnightPos]
in3 pos = do
  first  <- moveKnight pos
  second <- moveKnight first
  moveKnight second


canReachIn3 :: KnightPos -> KnightPos -> Bool
canReachIn3 start end = end `elem` in3 start
