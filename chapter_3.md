# すごいHな勉強会
## 第三章 - 関数の構文

### パターンマッチ
関数の場合わけに使う構文。以下の例を見れば多分わかるはず。
```
lucky :: Int -> String
lucky 7 = "LUCKY NUMBER SEVEN!"
lucky x = "Sorry, you are out of luck."
```
`x`を後に持ってくることに注意。でないと`7`を入力しても`x`にマッチするため全て`Sorry, ...`と出力されることになる(一応warningは出るみたい)。  

再帰も使える。再帰については4章でじっくりやる。
```
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
```
パターンにマッチしないものはエラーとなる。
```
// 定義部分
charName :: Char -> String
charName 'a' = "Alice"
charName 'b' = "Bob"
charName 'c' = "Charlie"

// ghci
*Main> charName 'a'
"Alice"
*Main> charName 'b'
"Bob"
*Main> charName 'c'
"Charlie"
*Main> charName 'd'
"*** Exception: chapter_3.hs:(10,1)-(12,24): Non-exhaustive patterns in function charName
```

#### タプルのパターンマッチ
2つの2次元空間のベクトル(ペア)を受け取って、それらを足し合わせる関数を書きたいとする。  
この場合以下のように**も**書ける
```
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors a b = (fst a + fst b, snd a + snd b)
```
パターンマッチでもっと簡潔に書くことができる。
```
addVectors :: (Double, Double) -> (Double, Double) -> (Double, Double)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)
```
このタプルのパターンマッチを使うことで1章でちらっと述べたトリプル以上の場合を定義することができる。
```
first :: (a, b, c) -> a
first (a, _, _) = a

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_, _, c) = c
```
