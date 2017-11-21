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

#### リストのパターンマッチ
リストには`1:[2,3] == [1,2,3]`という書き方がある。  
そのため`(x:xs) = [何かしらのリスト]`という形でxにリストの先頭の要素、xsに残りのリストの要素を束縛することができる。  
これを利用してパターンマッチを行う。
```
// headの例
head' :: [a] -> a
head' [] = error "empty list!"
head' (x:_) = x
```
#### asパターン
asパターンという特殊なパターンマッチがある。
asパターンは値をパターンに分解しつつ値自体も表現したい時に使う。  
以下例
```
firstLetter :: String -> String
firstLetter "" = "Empty String!"
firstLetter all@(x:xs) = [x] ++ " is the first letter of " ++ all
```

### 場合分けして、きっちりガード！
引数の値が満たす性質で場合分けする際には**ガード**を使う。
```
bmiTell1 :: Double -> String
bmiTell1 bmi
  | bmi <= 18.5 = "too light!"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat!"
  | otherwise   = "whale!"
```
if式でも表現できないことはないが、長くなると可読性が落ちるのでこっちの方が良い。  
当然だが複数の引数でも使える。
```
bmiTell2 :: Double -> Double -> String
bmiTell2 w h
  | w / h^2 <= 18.5 = "too light!"
  | w / h^2 <= 25.0 =  "normal"
  | w / h^2 <= 30.0 =  "fat!"
  | otherwise       = "whale!"
```

### where?
whereを使うことで中間結果に名前をつけることができる。
```
bmiTell3 :: Double -> Double -> String
bmiTell3 w h
  | bmi <= 18.5 = "too light!"
  | bmi <= 25.0 = "normal"
  | bmi <= 30.0 = "fat!"
  | otherwise    = "whale!"
  where bmi = w / h^2
```

#### whereのスコープ
whereはその関数内からでしか見えない。  
パターンマッチの際もそうである。  
例えば以下の例はコンパイルエラーとなる。
```
whereSample :: String -> String
whereSample "" = hoge
whereSample xs = hoge ++ " xs"
  where hoge = "hogehoge"
```

### let it Be
let式の紹介。  
let式はwhere節と似ている。違いはwhereは関数の終わりで変数を束縛することに対してlet式はどこでも変数を束縛でき、let式自体も式になる。スコープはガード間に跨らない。  
`let ** in **`の形をとる。
```
cylinder :: Double -> Double -> Double
cylinder r h =
  let sideArea = 2 * pi * r * h
      topArea = pi * r^2
  in sideArea + 2 * topArea
```
他にもいくつか例を紹介
```
// ローカルのスコープで関数を定義
*Main> let square x = x^2 in map square [1..10]
[1,4,9,16,25,36,49,64,81,100]

// セミコロン区切りで複数の変数を束縛
*Main> let a = 10; b = 20; c = 30 in a + b + c
60

// let式とタプルのパターンマッチ
*Main> let (a, b, c) = (10, 20, 30) in a + b + c
60
```
#### リスト内包表記でのlet
リスト内包表記でもlet式を使うことができる。以下はBMI計算関数の例。
```
calcBmis :: [(Double, Double)] -> [Double]
calcBmis xs = [bmi | (w, h) <- xs, let bmi = w / h^2]
```
この場合、`in`は使わない。bmiが`in`の後の値としての出力となり、`|`の前でも見える。

### case式
switch文のcaseに似ているが、違いは必ず値を返すこと。
以下は構文となる。
```
case *** of *** -> ***
            *** -> ***
            *** -> ***
            ...
```
以下は実際の例
```
describeList :: [a] -> String
describeList ls =
  "The list is "
  ++ case ls of [] -> "empty."
                [x] -> "a singleton list."
                _ -> "a longer list."
```
