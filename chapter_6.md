# すごいHな勉強会
## 第六章 - モジュール
Haskellのプログラムはモジュールの集合である。
これまで使ってきた全ての関数及び型、型クラスはデフォルトでインポートされるPreludeというモジュールの一部である。　　

### モジュールのインポート
`import`を使おう！
```
import Data.List
```
こうする事で`Data.List`が提供する全ての関数を使う事ができる。
`Data.List`は良く使うモジュールの一つ。リストから重複する要素を除く`nub`や`sort`等がある。
もしインポートする関数を直接指定するなら以下のように記述可能。
```
import Data.List (nub, sort)
```

他にも`import`の記述方法を紹介。
```
// nub関数以外を全てインポート
import Data.List hiding (nub)

// 名前の競合を避ける修飾付きインポート(PreludeとData.Mapではfilter関数等の名前が競合する事に注意)
import qualified Data.Map

// モジュールに別名をつける
import qualified Data.Map as M
```

### 標準モジュールの関数で問題を解く
モジュールを使った例をいくつか紹介する。
#### 単語を数える
`words`, `group`, `sort`関数を使った例。
文字列中に出現する各単語が何回現れるかを求める。

```
// words関数
Prelude Data.List> words "hey these are the words in this sentence"
["hey","these","are","the","words","in","this","sentence"]

// group関数
Prelude Data.List> group ["boom", "bip", "bip", "boom", "boom"]
[["boom"],["bip","bip"],["boom","boom"]]

// sort関数
Prelude Data.List> sort ["boom", "bip", "bip", "boom", "boom"]
["bip","bip","boom","boom","boom"]
```
出現単語数を求めるにはwords関数で文章を単語のリスト化してからsortしてgroup化…と手順を踏めばできそう。  
```
wordNums ::String -> [(String, Int)]
wordNums = map (\ws -> (head ws, length ws)) . group . sort . words
```

#### 正格な左畳み込みにて
五章で紹介した`foldl`はHaskellの遅延評価の特性上メモリを多く消費する。  
```
Prelude> foldl (+) 0 (replicate 100 1)
100
Prelude> foldl (+) 0 (replicate 10000000 1)
// stackoverflowになることもある
```
ここで`foldl (+) 0 [1,2,3]`がどのように遅延評価されるかを見てみる。
```
foldl (+) 0 [1,2,3] =
foldl (+) (0+1) [2,3] =
foldl (+) (0+1+2) [3] =
foldl (+) ((0+1)+2) [3] =
foldl (+) (((0+1)+2)+3) [] =
(((0+1)+2)+3) =
((0+1)+2)+3 =
(1+2)+3 =
3+3 =
6
```
2番目の引数のアキュームレータの計算が先延ばしにされている事がわかる。  
このアキュームレータの計算の先延ばしをやめるためにはData.Listの`foldl'`関数を使えば良い。(`foldr'`はData.Foldableにある)  
foldl'を使った場合の評価は以下のようになる。  
```
foldl' (+) 0 [1,2,3] =
foldl' (+) 1 [2,3] =
foldl' (+) 3 [3] =
foldl' (+) 6 [] =
6
```
スタックに積み上がる量が少し減るため、大きな値を扱うときはfoldl'を使うと良い。  

#### かっこいい数を見つけよう
各桁の数の合計値がnになる最初の自然数を求める。`digitToInt`, `find`関数を使って実装する。  
まずはdigitToInt, findの例。
```
*Main> :t digitToInt
digitToInt :: Char -> Int
*Main> digitToInt '9'
9

// find関数はMaybe a型の値を返す
*Main> find (>5) [1..10]
Just 6
*Main> find (==5) [1..10]
Just 5
*Main> find (>11) [1..10]
Nothing
```

ここでdigitToInt, findを使って実装してみる。
```
// 各桁の数を足し合わせる関数
digitSum :: Int -> Int
digitSum = sum . map digitToInt . show

// 各桁の数の合計値がnになる最初の自然数を求める関数
firstTo :: Int -> Maybe Int
firstTo n = find (\x -> digitSum x == n) [1..]
```

##### Maybe a型？
Maybe a型は失敗するかもしれない計算に使う型。  
aは多相型でどの型にでもなり得る。Maybe単体では型になりえないので`Maybe+何かしらの型`で初めて一つの型になる。  
計算に失敗した場合は`Nothing`を返し、成功した場合は`Just 10`, `Just "hoge"`等のMaybeに続く型に応じた値がJustに包まれて返る。

### モジュールを作ってみよう
今度はモジュールを作ってみる。モジュールを作るには以下のように書く。簡単。
```
// Geometry.hs
module Geometry
( sphereVolume,
, sphereArea
) where

sphereVolume :: Float -> Float
sphereVolume radius = (4.0 / 3.0) * pi * (radius ^ 3)

sphereArea :: Float -> Float
sphereArea radius = 4 * pi (radius ^ 2)
```

使うときとはGeometry.hsと同じフォルダ内で`import Geometry`とする。
