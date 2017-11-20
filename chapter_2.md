# すごいHな勉強会
## 第二章 - 型を信じろ!

### 明示的な型宣言
ghciで`:t`を使うことで変数、関数の型を確認することができる
```
Prelude> :t 'a'
'a' :: Char
Prelude> :t True
True :: Bool
Prelude> :t "Hello"
"Hello" :: [Char]
Prelude> :t (True, 'a')
(True, 'a') :: (Bool, Char)
```
`::`は「の型を持つ」と読む(らしい)。型注釈。  
`::`を使って関数に明示的な型宣言を行うこともできる。  
```
// 一章で出てきたdoubleMe, doubleUsに型宣言をしている例
doubleMe :: Int -> Int
doubleMe x = x * 2

doubleUs :: Int -> Int -> Int
doubleUs x y = doubleMe x + doubleMe y
```
上記の型宣言の意味はdoubleMeの場合「Int型の引数を一つとり、Int型の値を返す」となる。  
doubleUsの場合は「Int型の引数を二つとり、Int型の値を返す」となる。  

### 一般的なHaskellの型
以下によく使うHaskellの型をいくつか列挙する
- Int  
  整数(有界)
- Integer  
  整数(有界でない)
- Float
- Double
- Bool
- Char
- タプル  

\* タプルは型ではあるが厳密には要素の数とそれぞれの型によって決まる。空のタプル`()`も型である。  

### 型変数
例として`head`の型宣言を見てみる。  
```
Prelude> :t head
head :: [a] -> a
```
この`a`は**型変数**と呼ばれる。どのような型も取り得ることを意味する。  
型変数を用いた関数は多相的関数と呼ばれる(名前覚える必要ないけど一応)  

### 型クラス 初級講座
**型クラス**は何らかの振る舞いを定義するインタフェースである。  
ある型クラスのインスタンスである型は、その型クラスが提供する最低限のメソッドを実装してある。  

Eq型クラスについて考える。
```
Prelude> :i Eq
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  {-# MINIMAL (==) | (/=) #-}
```
Eq型クラスのインスタンスであるためには、インスタンスとなるための型が最低限`==`または`/=`を実装している必要がある。  
例えばIntについて見てみる。  
```
Prelude> :i Int
data Int = GHC.Types.I# GHC.Prim.Int#   -- Defined in ‘GHC.Types’
instance Bounded Int -- Defined in ‘GHC.Enum’
instance Enum Int -- Defined in ‘GHC.Enum’
instance Eq Int -- Defined in ‘GHC.Classes’
...以下続く
```
IntはEqのインスタンスであると書いてある。  
これにより、Intは`==`を実装していることがわかる。そのため、以下のような演算ができる。  
```
Prelude> 4 == 4
True
Prelude> 4 /= 4
False
Prelude> 4 /= 5
True
```
以下より一般的な型クラスの紹介をする

#### Ord型クラス
順序比較ができる型のための型クラス。`compare`を実装していればインスタンスになれる。  
```
Prelude> :t compare
compare :: Ord a => a -> a -> Ordering
Prelude> compare 4 5
LT
Prelude> compare 4 4
EQ
Prelude> compare 4 3
GT

// 比較演算子もOrd型クラスのメソッド
Prelude> 4 > 1
True
Prelude> 4 < 10
True
Prelude> 4 <= 10
True
```

#### Show型クラス
ある値は、その型がShow型クラスのインスタンスであれば文字列として表現できる。  
```
Prelude> show 4
"4"
Prelude> show True
"True"
Prelude> show "hoge"
"\"hoge\""
```

#### Read型クラス
Showと対をなす型クラス。文字列を受け取ってRead型クラスのインスタンスのうちどれかの型として値を返す。
```
Prelude> read "True" || False
True
Prelude> read "8.2" + 3.8
12.0
Prelude> read "5" -2
3
Prelude> read "[1,2,3,4]" ++ [5]
[1,2,3,4,5]
```
コンパイラが型を推論できる形にしてやらないとエラーとなる。
```
Prelude> read "8"
*** Exception: Prelude.read: no parse
```
`"8"`だけだと何を返せばいいのかわからない。
もし単体で使いたいなら明示的に型注釈の`::`で型を指定してやる必要がある。
```
Prelude> read "8" :: Int
8
Prelude> read "True" :: Bool
True
```
型を知るための最小限の文脈さえ分かればコンパイラは型推論ができる。
```
Prelude> [read "True", False, True]
[True,False,True]
```

#### Enum型クラス
列挙型。順番に並んだ値を列挙できる型。  
`succ`や`pred`関数がEnum型クラスのインスタンスの定義する関数。  
```
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> ['a'..'e']
"abcde"
Prelude> succ 4
5
Prelude> pred 4
3
```

#### Bounded型クラス
この型クラスのインスタンスは上限と加減を持ち、それぞれminBoundとmaxBound関数で調べられる。  
```
Prelude> minBound :: Word
0
Prelude> maxBound :: Word
18446744073709551615
Prelude> minBound :: Int
-9223372036854775808
Prelude> maxBound  :: Int
9223372036854775807
Prelude> minBound  :: Char
'\NUL'
Prelude> maxBound  :: Char
'\1114111'
```

#### Num型クラス
数の型クラス、この型クラスのインスタンスは数のように振る舞う。  
`+`, `-`, `*`などの演算子がこの型クラスのメソッドとなる。  

#### 最後に - 型クラスに関するいくつかの注意点
型クラスは他の型クラスのインスタンスとして定義もできる。  
型をある型クラスのインスタンスにするために、まずは別の型クラスのインスタンスにする必要がある場合がある。  

(例)  
Ord型クラスのインスタンスとなるためにはEq型クラスのインスタンスでなえればならない。  
大小比較するためには等値比較もできなきゃダメだよっていうこと。
