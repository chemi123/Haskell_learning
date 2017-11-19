# すごいHな勉強会
## 第一章 - はじめの第一歩

### ghciの簡単な演算例
```
// あえて紹介するほどでもないけど一般的な演算の例
Prelude> 2 + 15
17
Prelude> 49 * 100
4900
Prelude> 1892 - 1472
420
Prelude> 5 / 2
2.5
Prelude> 50 * 100 - 4999
1
Prelude> 50 * (100 - 4999)
-244950
Prelude> True && False
False
Prelude> True || False
True
Prelude> not (True || False)
False
Prelude> 5 == 5
True

// not equalは少し特殊
Prelude> 5 /= 5
False
```

### 簡単な関数の紹介
```
// Haskellでは引数を括弧に包まない。スペース区切りで判断する
Prelude> succ 8
9
Prelude> min 9 10
9
Prelude> max 9 10
10

// 演算子によって右結合、左結合の優先順位がある。
// 関数の優先順位が一番高い
// 詳しくは http://walk.northcol.org/haskell/operators を参照
Prelude> succ 9 + max 5 4 + 1
16
Prelude> (succ 9) + (max 5 4) + 1
16
```

### 関数定義
以下関数をbaby.hsに定義する
```
doubleMe x = x * 2
```

doubleMe関数をghciで使うなら以下のようにファイルをロードする
```
Prelude> :l baby.hs
[1 of 1] Compiling Main             ( baby.hs, interpreted )
Ok, modules loaded: Main.

// これでbaby.hsに定義されたdoubleMe関数を使うことができる
*Main> doubleMe 2
4

// 特に型宣言をしてないので型推論が行われる
*Main> doubleMe 1.2
2.4
```

doubleMe関数を使って別のdoubleUs関数を定義もできる
```
doubleUs x y = doubleMe x + doubleMe y
```

```
// 使い方は簡単
*Main> doubleUs 1 2
6
*Main> doubleUs 1.4 2
6.8
```

```
// if文を使った例
doubleSmallNumber x = if x > 100  
                        then x  
                        else x*2

// 一行で定義(Haskellは関数名にクォートを使うことができる)
doubleSmallNumber' x = (if x > 100 then x else x*2) + 1
```

### リスト
```
Prelude> let a = [1,4,7,9,2]
Prelude> a
[1,4,7,9,2]

// 要素へアクセス
Prelude> a !! 0
1
Prelude> a !! 1
4

// ランタイムエラーになるので注意!!
Prelude> a !! 5
*** Exception: Prelude.!!: index too large

// リストの結合
Prelude> [1,4,7,9,2] ++ [3,5]
[1,4,7,9,2,3,5]

// リスト中のリスト
Prelude> b = [[1,2,3],[4,5,6],[7,8,9]]
[[1,2,3],[4,5,6],[7,8,9]]
Prelude> b !! 0
[1,2,3]
Prelude> b !! 0 !! 1
2
```

### さらなるリスト操作
```
Prelude> head [5,4,3,2,1]
5
Prelude> tail [5,4,3,2,1]
[4,3,2,1]
Prelude> last [5,4,3,2,1]
1
Prelude> init [5,4,3,2,1]
[5,4,3,2]

// ランタイムエラーになるので注意!!
Prelude> head []
*** Exception: Prelude.head: empty list

Prelude> length [5,4,3,2,1]
5

Prelude> null [5,4,3,2,1]
False
Prelude> null []
True

Prelude> reverse [5,4,3,2,1]
[1,2,3,4,5]

Prelude> take 2 [5,4,3,2,1]
[5,4]
Prelude> take 4 [5,4,3,2,1]
[5,4,3,2]
Prelude> take 10 [5,4,3,2,1]
[5,4,3,2,1]
Prelude> take (-1) [5,4,3,2,1]
[]

Prelude> drop 1 [5,4,3,2,1]
[4,3,2,1]
Prelude> drop 3 [5,4,3,2,1]
[2,1]
Prelude> drop 100 [5,4,3,2,1]
[]

Prelude> elem 10 [5,4,3,2,1]
False
Prelude> elem 3 [5,4,3,2,1]
True
Prelude> 5 `elem` [5,4,3,2,1] -- 中置記法としても書ける
True

// 他にもmaximum, minimum, sum, product
```

### レンジ
```
Prelude> [1..10]
[1,2,3,4,5,6,7,8,9,10]
Prelude> ['a'..'z']
"abcdefghijklmnopqrstuvwxyz"
Prelude> ['K'..'Z']
"KLMNOPQRSTUVWXYZ"

// 等差数列みたいにも記述できる
Prelude> [1,3..21]
[1,3,5,7,9,11,13,15,17,19,21]

// 無限リスト
Prelude> [1..]
[1,2,3,4,5,6,7,8,9,10,11,12....無限に続く

// Haskellは遅延評価なので実際に評価される時にしか計算しない
Prelude> a = [1..]
Prelude> take 10 a
[1,2,3,4,5,6,7,8,9,10]
```

### リスト内包表記
```
// 数学の集合に似てる
// 無事に勉強会が進めば説明するが実はリストモナドによって実現されている
Prelude> [x*2 | x <- [1..10]]
[2,4,6,8,10,12,14,16,18,20]

// 条件の追加
Prelude> [x | x <- [50..100], x `mod` 7 == 3]
[52,59,66,73,80,87,94]

// x, yの全組み合わせ
Prelude> [x*y | x <- [1..3], y <- [6..10]]
[6,7,8,9,10,12,14,16,18,20,18,21,24,27,30]

// リスト内包表記による関数定義+アンダースコア(_)の例
Prelude> length' xs = sum [1 | _ <- xs]
Prelude> length' [1..10]
10
```

### タプル
以下2点はリストとタプルの違いである
1. タプルはヘテロである(複数の違う型を格納できる)
2. タプルはサイズ固定(格納する要素数を事前に知っている必要がある)  

```
// タプル例
Prelude> (1, 3)
(1,3)
Prelude> (1, 'a', "hoge")
(1,'a',"hoge")
```

### タプルを使う
```
// リストで扱う場合はタプルの要素数同じである必要がある
Prelude> [(1, 2, 3), (1, 2, 4)]
[(1,2,3),(1,2,4)]
Prelude> [(1, 2, 3), (1, 2)]

<interactive>:48:13: error:
    • Couldn't match expected type ‘(t2, t1, t)’
                  with actual type ‘(Integer, Integer)’
    • In the expression: (1, 2)
      In the expression: [(1, 2, 3), (1, 2)]
      In an equation for ‘it’: it = [(1, 2, 3), (1, 2)]
    • Relevant bindings include
        it :: [(t2, t1, t)] (bound at <interactive>:48:1)

// 型も同じである必要がある
Prelude> [(1, "hoge"), (2, "fuga"), (3, "piyo")]
[(1,"hoge"),(2,"fuga"),(3,"piyo")]
Prelude> [(1, "hoge"), (2, "fuga"), (3, 'p')]

<interactive>:52:32: error:
    • Couldn't match expected type ‘[Char]’ with actual type ‘Char’
    • In the expression: 'p'
      In the expression: (3, 'p')
      In the expression: [(1, "hoge"), (2, "fuga"), (3, 'p')]
```

### ペア
```
// ペアのタプルならfst, sndが使える
Prelude> fst (1, 2)
1
Prelude> snd (1, 2)
2
```
トリプル以上のタプルの場合は自分で関数を定義する必要がある。  
その場合はパターンマッチという構文が出てくるのでまた今後説明する。

### おまけ
Haskellで調べものをする際には[hoogle](https://www.haskell.org/hoogle/)+[hackage](http://hackage.haskell.org/)が良い
