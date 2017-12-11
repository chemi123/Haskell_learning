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


foldl' :: (a -> b -> a) -> a -> [b] -> a
foldl' f acc [] = acc
foldl' f acc (x:xs) = foldl' f (f acc x) xs


foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' f acc [] = acc
foldr' f acc (x:xs) = f x (foldr' f acc xs)


suml :: (Num a) => [a] -> a
suml = foldl (+) 0


sumr :: (Num a) => [a] -> a
sumr = foldr (+) 0


mapl :: (a -> b) -> [a] -> [b]
mapl f xs = foldl (\acc x -> acc ++ [f x]) [] xs


mapr :: (a -> b) -> [a] -> [b]
mapr f xs = foldr (\x acc -> f x : acc) [] xs
