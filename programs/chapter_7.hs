data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float deriving (Show)

area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)


--data Person = Person String String Int Float String String deriving (Show)
--
--
--firstName :: Person -> String
--firstName (Person firstname _ _ _ _ _) = firstname
--
--
--lastName :: Person -> String
--lastName (Person _ lastname _ _ _ _) = lastname
--
--
--flavor :: Person -> String
--flavor (Person _ _ _ _ _ flavor) = flavor


data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)


data Vector a = Vector a a a deriving (Show, Eq)

vplus :: Num a => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

dotProd :: Num a => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = (i*l) + (j*m) + (k*n)

vmult :: Num a => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)


data Day = Monday | Tuesday | Wednesday |
           Thursday | Friday | Saturday | Sunday
           deriving(Eq, Ord, Show, Bounded, Enum)


maybeFactorial :: Int -> Maybe Int
maybeFactorial n
  | n < 0     = Nothing
  | n <= 1    = Just n
  | otherwise = fmap (*n) $ maybeFactorial (n-1)


eitherFactrorial :: Int -> Either String Int
eitherFactrorial n
  | n < 0     = Left "can not take negative number" 
  | n <= 1    = Right n
  | otherwise = (*n) <$> eitherFactrorial (n-1)


data List a = Empty | List a (List a) deriving (Eq, Ord, Show, Read)


data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)


instance Functor Tree where
  fmap f EmptyNode = EmptyNode
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)


singleton :: a -> Tree a
singleton x = Node x EmptyNode EmptyNode


insertNode :: (Ord a) => a -> Tree a -> Tree a
insertNode x EmptyNode = singleton x
insertNode x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (insertNode x left) right
  | x > a  = Node a left (insertNode x right)


treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyNode = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right


data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False

instance Show TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"


class YesNo a where
  yesno :: a -> Bool


instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True
