# すごいHな勉強会
## 第四章 - Hello 再帰!

### 最高に最高!
再帰関数定義のための基本はパターンマッチを用いる。 まずはmaximum関数を例にとる。
```
maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list!"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
```
Ord型のリストを受け取り、その中で最も大きい値を返す関数。  
最初にリストが空だった場合はエラーを返すように定義(本当はよくない)。  

### さらにいくつかの再帰関数
いくつか標準ライブラリにある関数を紹介。
#### replicate
replicateはIntと値を受け取り、値を指定された数だけ繰り返したリストを返す。  
例えばreplicate 3は、3つの5からなるリストを返す。  
```
replicate' :: Int -> a -> [a]
replicate' n x
  | n <= 0 = []
  | otherwise = x : replicate' (n-1) x
```

#### take
take関数は指定された値の数だけ指定されたリストから取り出す。
```
take' :: Int -> [a] -> [a]
take' n _
  | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs
```

#### zip
2つのリストを引数にとり、これらをとじ合わせてペアのリストにして返す。  
```
zip' :: [a] -> [b] -> [(a, b)]
zip' _ [] = []
zip' [] _ = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys
```

#### elem
値とリストを受け取り値がリストの要素に含まれているのであればTrue、それ以外であればFalseを返す。
```
elem' :: (Eq a) => a -> [a] -> Bool
elem' a [] = False
elem' a (x:xs)
  | a == x    = True
  | otherwise = a `elem'` xs
```
