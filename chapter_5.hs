multTree :: Int -> Int -> Int -> Int
multTree x y z = x * y * z


applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)


zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] _ = []
zipWith' f _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys


map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f (x:xs) = f x : map' f xs


filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f (x:xs)
  | f x = x : filter' f xs
  | otherwise = filter' f xs
