# すごいHな勉強会
## 第十二章 - モノイド
### 既存の型を新しい型にくるむ
これまでdataを使って新しい型を作る方法やtypeによって既存の型に型シノニムを与える方法を学んできた。  
ここでは**newtype**キーワードを使って既存の型から新たな作る方法及び、なぜnewtypeキーワードが必要かをZipListを例に見ていく。

もし何も知らない状態でZipListを定義しようと思ったら恐らく以下のように定義する。

```
data ZipList a = ZipList { getZipList :: [a] }
```

そしてFunctor, Applicative Functorのインスタンスとして定義をしていくという手順を踏む。しかしZipListは実際には以下のように定義されている。

```
newtype ZipList a = ZipList { getZipList :: [a] }
```

dataでも問題ないのにnewtypeを使う理由はnewtypeのほうが高速だからである。  
ZipListは実際にはリストを違う振る舞いにしたいために定義されているが、もしdataでリストを包むとコンストラクタに包んだりほどいたりするたびにオーバーヘッドがかかる(どういうことかあまりよくわかってない)。  

しかしnewtypeを使えば内部で実際には同じ型であるがアプリカティブ等での振る舞いを変えたいために便宜上違う型として表されているだけだとよろしく解釈してくれるため包んだりほどいたりするという処理を省略してくれるらしい。  

ではなぜ逆に常にdataでなくnewtypeを使わないのであろうか？  
その理由はnewtypeの場合は以下2つの制約があるためである。
1. 値コンストラクタを一つしか持つことができない
2. 値コンストラクタが持てるフィールドは一つだけである

この制約のため、newtypeとdataは使い分けられる。
ちなみにnewtypeで作った型に対してderivingを使えばEq等のインスタンスを自動導出することもできる。  
ただしnewtypeで包んだ型はその型クラスのインスタンスである必要がある。

#### newtypeを使って型クラスのインスタンスを作る
ある型を何かしらの型クラスのインスタンスにしたいが型引数が一致しなくてできないというケースがままある。  
例えばMaybeをFunctorの型クラスにしたい場合は以下のように簡単にすることができる。

```
instance Functor Maybe where
  fmap :: (a -> b) -> Maybe a -> Maybe b
```

ここでまず2つの要素を持つタプルをfmapで写すと以下のよう2つ目の要素に関数が適用される。

```
Prelude> fmap (+1) (1,2)
(1,3)
```

これは2つの要素を持つタプル限定でFunctorのインスタンスが以下のように定義されるからである。(3つ以上の場合は数に応じて定義しなければならない)

```
// (,)は2つの要素を持つタプルの値コンストラクタ。発見した時は結構衝撃だった
data (,) a b = (,) a b

instance Functor ((,) a) where
  fmap f (a, b) = (a, f b)
```

もし1つ目の要素を写したくてもFunctorのインスタンス宣言で型引数を一つだけとるような型コンストラクタを渡さなければならないのだが、構造上できないことがわかる。  
そういう場合にnewtypeを使って新しい型を作ることで解決することができる。

```
newtype Pair b a = Pair { getPair :: (a, b) }

instance Functor (Pair b) where
  fmap f (Pair (x, y)) = Pair (f x, y)

// 上記で定義されたPairに対してfmapを試してみる
*Main> getPair $ fmap (+1) $ Pair (1, 2)
(2,2)
```

タプルが三つ以上の場合も頑張れば作れる(あまり頑張る場面が必要な場合が想像できないが)。以下は遊びで作ってみた。

```
instance Functor ((,,) a b) where
  fmap f (a, b, c) = (a, b, f c)

newtype Triple b c a = Triple { getTriple :: (a, b, c) } deriving (Show, Eq, Ord)

instance Functor (Triple c d) where
  fmap f (Triple (x, y, z)) = Triple (f x, y, z)

// 実行
*Main> fmap (+1) (1,1,1)
(1,1,2)
*Main> getTriple $ fmap (+1) $ Triple (1,1,1)
(2,1,1)
```

#### type vs newtype vs data
ここでtype, newtype, dataの使い方について整理する。  

##### type
型シノニムを作るためのもの。  
既存の型に型名をつけて呼びやすくするためのもの。例えば[Int]に別名を与えたい場合以下のように書ける。

```
type IntList = [Int]
```

IntListと[Int]は全く同じ型を表している。要するにエイリアスのこと。

##### newtype
既存の型を包んでで新しい型を作るためのもの。  
既存の型を包むと言ってもできた型は違う型として認識される。  

dataを使うよりも処理が速く、既存のインスタンスと少し振る舞いを変えたい場合に使う。  
しかしnewtypeの場合は値コンストラクタ及びそのフィールドは1つまでという制約がある。

##### data
全く新しい自前のデータ型を作りたい時に使う。

### Monoid大集合
Haskellの型クラスは、同じ振る舞いをする型たちに共通のインターフェースを提供するために使われる。  
例えばEq型クラスは等号という機能を、Ord型クラスは比較という機能を提供する。  
型クラスを作る際には「この型には何ができ、どのような操作をサポートするのだろう？」と考え、その型の欲しい機能を元にどの型クラスのインスタンスを実装するかを決める。ある型を作る際に等値比較をする必要があるならEq型クラスのインスタンスにする、など。

唐突だがここで関数\*について考えてみる。\*は2つの数をとって掛け算をする関数である。
そして何かしらの数と1を掛け算すると答えは常に元の数になる。  
`1 * x`及び`x * 1`は必ずxになるという性質を持つ。  

次に関数++について考える。これはリストを2つ取って結合する関数である。  
x \* 1のように元の値を返す何かしらの値はリストでは[]があたる。

```
// 演算の結果相手の値を変えない例
*Main> 4 * 1
4
*Main> 1 * 4
4
*Main> "hey" ++ []
"hey"
*Main> [1..10] ++ []
[1,2,3,4,5,6,7,8,9,10]
```

これだけ見ると\*と++には以下のような共通の性質があると考えられる。
* 関数は引数を2つとる
* 2つの引数及び返り値の型は全て等しい(a -> a -> a)
* 2引数関数を施しても相手を変えないような特殊な値が存在する

よく観察すると他にも共通の性質があることがわかる。  
この関数を使って3つ以上の値を1つの値にまとめる計算をする時、値の間に関数を挟む順序を変えても結果は変わらないという性質である。

```
*Main> 2 * (3 * 4)
24
*Main> (2 * 3) * 4
24
*Main> "hoge" ++ ("fuga" ++ "piyo")
"hogefugapiyo"
*Main> ("hoge" ++ "fuga") ++ "piyo"
"hogefugapiyo"
```

このような性質を**結合的**(associativity)と呼ぶ。  
これらの性質に気づいた時・・・それはモノイドとの出会いとなる。

#### Monoid型クラス
**モノイド**とは`結合的な二項演算子(2引数関数)と、その演算に関する単位元からなる構造`のことである。  
ある値がある演算の単位元であるとは、その値と何か他の値を引数にしてその演算を呼び出した時に返り値が常に他の値のほうと等しくなる、ということである。  
先の例だと1及び[]がそれにあたる。  
Haskellでは他にも無数のモノイドがあり、Monoid型クラスが用意されている。

```
class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty
```

Monoid型クラスの提供する関数の型宣言を見ると、Monoid型クラスのインスタンスは具体型であることが分かる。

memptyは単位元を示す。これは引数を取らないので関数ではない。  

次のmappendはモノイド固有の二項演算となる。mappendは同じ型の引数を2つ取り、その型の値を返す。  
++は確かにappendだと考えられるが、\*はappendとは程遠いものであるのでそう考えると残念な名前付けである。  

最後の関数はmconcatである。  
これはモノイドのリストを受け取り、mappendを間に挟んでmepmtyを初期値として右畳み込みをすることで単一の値を計算してくれる関数である。  
自分でMonoidのインスタンスを実装する際はmempty及びmappendだけ実装すればよい。

#### モノイド則
モノイドが満たすべき法則を紹介する。  
モノイドには固有の二項演算があり、その二項演算に関する単位元があり結合的であれば良い。よって以下の式で表される。
* mempty \`mappend\` x = x
* x \`mappend\` mempty = x
* (x \`mappend\` y) \`mappend\` z = x \`mappend\` (y \`mappend\` z)

### モノイドとの遭遇
モノイドのインスタンスを紹介していく。

#### リストはモノイド
先ほども少し触れたがリストはモノイドである。以下はインスタンス定義である。
```
instance Monoid [a] where
  mempty = []
  mappend = (++)
```

少し考えれば分かるが[]にどのようなリストを結合しても[]以外のリストと同じ値になり、(++)の結合の実行順序を変えたとしても同じ値になる。  
よってリストはモノイドである。以下は簡単な実行例。

```
*Main> [1..10] `mappend` []
[1,2,3,4,5,6,7,8,9,10]
*Main> [1..5] `mappend` [6..10]
[1,2,3,4,5,6,7,8,9,10]
*Main> mempty :: [a]
[]
```

ちなみにモノイド則はa `mappend` bとb `mappend` aが等しいことは要求してない。  
"hoge" `mappend` "piyo"と"piyo" `mappend` "hoge"は違うことを考えれば明らかである。

#### ProductとSum
数をモノイドにする一つの方法はすでに触れた通り\*を二項演算にして1を単位元にするという方法である。  
それだけではなく+を二項演算にして0を単位元として扱うという方法がある。

```
// +を二項演算、0を単位元としてもモノイドの性質を満たしていることが分かる
Prelude> 0 + 1
1
Prelude> 1 + 0
1
Prelude> 1 + (2 + 3)
6
Prelude> (1 + 2) + 3
6

// *も同様。復習
Prelude> 1 * 10
10
Prelude> 10 * 1
10
Prelude> 1 * (2 * 3)
6
Prelude> (1 * 2) * 3
6
```

Haskellでは当然のことながらこれらの性質を満たすモノイドのインスタンスが定義されている。それが**Sum**と**Product**になる。以下はSum及びProductの定義。

```
newtype Sum a = Sum {getSum:: a } deriving (Eq, Ord, Show, Read, Bounded)

instance Monoid (Sum a) where
  mempty = Sum 0
  Sum x `mappend` Sum y = Sum (x + y)

newtype Product a = Product {getProduct :: a } deriving (Eq, Ord, Show, Read, Bounded)

instance Monoid (Product a) where
  mempty = Product 1
  Product x `mappend` Product y = Product (x * y)
```

Sum及びProductのmemptyは単純に0及び1をがそれぞれ+と\*の単位元となるためSum,Productに包んでいる。  
mappendについては+と\*の機能をそれぞれに持たせているにすぎない。

```
// Sum
*Main> mempty :: Sum Int
Sum {getSum = 0}
*Main> Sum 1 `mappend` Sum 0
Sum {getSum = 1}
*Main> Sum 0 `mappend` Sum 1
Sum {getSum = 1}
*Main> Sum 1 `mappend` (Sum 2 `mappend` Sum 3)
Sum {getSum = 6}
*Main> (Sum 1 `mappend` Sum 2) `mappend` Sum 3
Sum {getSum = 6}

// Product
*Main> mempty :: Product Double
Product {getProduct = 1.0}
*Main> Product 1 `mappend` Product 2
Product {getProduct = 2}
*Main> Product 2 `mappend` Product 1
Product {getProduct = 2}
*Main> Product 2 `mappend` (Product 3 `mappend` Product 4)
Product {getProduct = 24}
*Main> (Product 2 `mappend` Product 3) `mappend` Product 4
Product {getProduct = 24}
```

上の例はモノイド則を満たしていることが分かる。

#### AnyとAll
Boolでもモノイドになり得る。  
`||`及び`&&`がそれにあたる。Any,Allの名前から推測できるがAnyは一つでもTrueがあればTrueになり、Allは一つでもFalseがあればFalseとなる。定義は以下になる。

```
newtype Any = Any { getAny :: Bool } deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid Any where
  mempty = Any False
  Any x `mappend` Any y = Any (x || y)

newtype All = All { getAll :: Bool } deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid All where
  mempty = All True
  All x `mappend` All y = All (x && y)
```

以下は使い方例。

```
// Any
*Main> Any False `mappend` Any True
Any {getAny = True}
*Main> Any True `mappend` Any False
Any {getAny = True}
*Main> Any False `mappend` (Any False `mappend` Any True)
Any {getAny = True}
*Main> (Any False `mappend` Any False) `mappend` Any True
Any {getAny = True}
```

#### Maybeモノイド
Maybe aもモノイドになれる。  
ただしMaybe aがモノイドになるには型aがモノイドでなければならないという型クラス制約があり、Nothingを単位元とする。以下がインスタンス定義となる。

```
instance (Monoid a) => Monoid (Maybe a) where
  mempty = Nothing
  Nothing `mappend` m = m
  m `mappend` Nothing = m
  (Just m1) `mappend` (Just m2) = Just (m1 `mappend` m2)
```

定義の中でm1 \`mappend\` m2ができるのはaがMonoid型クラスのインスタンスであるという型クラス制約があるからである。  

```
*Main> Nothing `mappend` Just [1..10]
Just [1,2,3,4,5,6,7,8,9,10]
*Main> Just [1..5] `mappend` Just [6..10]
Just [1,2,3,4,5,6,7,8,9,10]
*Main> Just (Sum 10) `mappend` Just (Sum 20)
Just (Sum {getSum = 30})
*Main> Just (Sum 10) `mappend` Nothing
Just (Sum {getSum = 10})
```

さらにMaybeを包んだFirstもあるがここでは割愛する。FirstはいくつかあるMaybe a型の値にどれか一つでもJustがあるかどうかを調べたい時に使う。

### モノイドで畳み込む
色々なデータ構造の上に畳み込みを定義したい...そんな時はモノイドが活躍する。  
今までリストの畳み込みしか紹介しなかったが、畳み込みできるデータ構造はリストだけではない。木構造などは畳み込みをしやすい典型例といえる。  
畳み込みと相性が良いデータ構造は多くあるため、Foldable型クラスが導入された。Functorが関数で写せるものを表すようにFoldableは畳み込みができるものを表している。  
早速Data.Foldableをインポートして確認してみる。  

```
*Main> :t foldr
foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
*Main> :t F.foldr
F.foldr :: Foldable t => (a -> b -> b) -> b -> t a -> b
```

*2012年時の本が出版された時点ではPreludeで実装されたfoldrではリストしか畳み込みできなかったと考えられる。

実はリストだけではなくFoldable型クラスのインスタンスであれば畳み込みができそうなことが分かる。  
MaybeやEitherはFoldable型クラスのインスタンスであるので試してみる。

```
Prelude> foldr (+) 4 $ Just 10
14
Prelude> foldr (+) 10 $ Right 10
20
```

畳み込みできることはできるが何も面白くない。  
木構造で畳み込みができたら面白いはずなので木構造で畳み込みができるよう頑張ってみる。  
七章で出て来た木構造の定義は以下となる。

```
data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving Show
```

この木構造を畳み込みするためにはFoldable型クラスのインスタンス定義をしてやれば良さそうなことが分かる。  
Foldable型クラスの最小完全定義はfoldMap又はfoldrとなる。  
教科書曰くfoldMapの方がfoldrよりも簡単なようなのでfoldMapを実装してみることにする。foldMapの型宣言は以下のようになる。

```
*Main> :t foldMap
foldMap :: (Monoid m, Foldable t) => (a -> m) -> t a -> m
```

畳み込みにはモノイドが活躍すると述べた理由はここにある。  
この型宣言を見ると何かしらの型をとってモノイドを返す関数を第一引数に取り、第二引数にFoldable型のインスタンス型の値をとることでモノイドを返すことが分かる。  

foldMapの動作はFoldableのインスタンス型に包まれた値をモノイドに変換してからmappendで結合していき、最後に得られたモノイド値を返すというものになる。以下は定義。

```
instance Foldable Tree where
  foldMap f EmptyNode = mempty
  foldMap f (Node x left right) = foldMap f left `mappend`
                                  f x            `mappend`
                                  foldMap f right
```

foldMapの結果はモノイド値になることがわかっているのでleft及びrightは再帰的にfoldMapの処理を行い、xにはfを適用してモノイド値を得る。  
そして最終的に得られたモノイド値の群をmappendしていくことで単一のモノイド値となる。  

f及びmemptyがどのようなモノイド値になることが重要なのではなく、どのような順番で適用していくのかが重要なのでfやmemptyがどのようなモノイド値を返すかは特に定義には出て来ない。

さてこれでなぜfoldrができるかというと以下のように自動的に定義ができるからである。

```
// 厳密には違うが個人的にわかりやすいと思った定義
foldr f z t = foldr ($) z $ foldMap (\x -> [f x]) t

// 本当の定義
foldr f z t = appEndo (foldMap (Endo . f) t ) z
```

個人的にわかりやすいと思った定義でこのTreeを例にして説明する。  
foldMapでまずはTree型の値のNodeの中身に2引数関数fを適用してリストに格納。リストはもちろんモノイドなのでmappendで結合できる。そのように再帰的に結合した結果一引数関数のリストが出来上がる。($)は`(($) :: (a -> b) -> a -> b)`で第一引数に1引数関数をとり第二引数にその関数の引数と同じ型の値をとり第二引数に第一引数の関数を適用して結果を返す関数である。  
なのであとはリストのfoldrと同じになるのでリスト内の関数を初期値zで右畳み込みして結果を返す。

```
// treeを以下のように定義
tree = Node 10 (Node 5 EmptyNode (Node 8 EmptyNode EmptyNode)) (Node 20 (Node 15 EmptyNode EmptyNode) EmptyNode)

*Main> foldr (+) 0 tree
58
*Main> foldr (*) 1 tree
120000

// もちろんfoldlも動く
*Main> foldl (*) 1 tree
120000
```

ちなみにfoldMapはFoldable型クラスのインスタンス定義に使うだけでなくFoldableなインスタンス値を単一なモノイドに畳みたい時に便利な関数である。以下は使い方例。

```
// 8の要素を持つノードがあるかを探索
*Main> getAny $ foldMap (\x -> Any $ x == 8) tree
True

// リストに変換したい場合
*Main> foldMap (\x -> [x]) tree
[5,8,10,15,20]
```
