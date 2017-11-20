# すごいHな勉強会
## 第二章 - 型を信じろ!

### 明示的な型宣言
ghci`:t`を使うことで変数、関数の型を確認することができる
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
`::`は「の型を持つ」と読む(らしい)。  
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
