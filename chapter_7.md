# すごいHな勉強会
## 第七章 - 型や型クラスを自分で作ろう
これまでBook, Int, Char, Maybe等のデータ型やEq, Ord, Show等のいろいろな型クラスをみてきた。  
この章では独自の型及び型クラスについての作り型を見ていく。

### 新しいデータ型を定義する
自作のデータ型を作る方法の一つは`data`キーワードを使うことになる。  
まずは例として標準ライブラリのBool型の定義がどのようになっているかを見てみる。  

```
// Bool型の定義(厳密ではない)
data Bool = False | True
```

等号の後の`False`及び`True`は値コンストラクタと呼ばれる。`|`は`または`という意味になる。  
つまりこの定義が意味しているのは`Bool型はTrueまたはFalseの値を取り得る`というふうに読める。  
型コンストラクタと値コンストラクタは大文字で始まる必要がある。  
同様にInt型も以下のように定義されているとみなせる(もちろん厳密には違う)。

```
//data Int = -217483648 | -217483647 | ... | -1 | 0 | 1 | ... | 217483647
```

### 形づくる
長方形と円という2種類の図形を扱う例で試してみる。

```
data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float
```

これはCircle値コンストラクタの場合はFloat型の引数を3つとり、Rectangle値コンストラクタはFloat型の引数を4つとると解釈することができる。


```
*Main> :t Circle
Circle :: Float -> Float -> Float -> Shape
*Main> :t Rectangle
Rectangle :: Float -> Float -> Float -> Float -> Shape
```

`type`で型シグネチャを確認してみると値コンストラクタはただの関数であることがわかる。  
もちろんShapeを引数にとる関数を定義することもできる。

```
area :: Shape -> Float
area (Circle _ _ r) = pi * r ^ 2
area (Rectangle x1 y1 x2 y2) = (abs $ x2 - x1) * (abs $ y2 - y1)
```

このように値コンストラクタではパターンマッチを使うこともできる(`(Circle _ _ r) =`や`Rectangle x1 y1 x2 y2) =`の部分等みたいな感じで)。  

ちなみに`Circle 0 0 1`みたいな感じでプロンプトに表示しようとすると怒られる。

```
*Main> Circle 0 0 1

<interactive>:37:1: error:
    • No instance for (Show Shape) arising from a use of ‘print’
    • In a stmt of an interactive GHCi command: print it
```

これはShapeがShow型クラスのインスタンスでないため、プロンプトに表示をする方法がわからないというエラー。  
`deriving (Show)`と書けば表示することができるようになる。derivingは後で出てくるのでひとまず今はこれくらいで。

```
// ファイル内での定義
data Shape = Circle Float Float Float |
             Rectangle Float Float Float Float deriving (Show)

// GHCi
*Main> Circle 0 0 1
Circle 0.0 0.0 1.0
```

値コンストラクタは関数なのでmap関数を適用したり、部分適用ができる。  

```
// 半径の違う同心円を複数作る場合
*Main> map (Circle 0 0) [1..5]
[Circle 0.0 0.0 1.0,Circle 0.0 0.0 2.0,Circle 0.0 0.0 3.0,Circle 0.0 0.0 4.0,Circle 0.0 0.0 5.0]
```

### レコード構文
次はデータ型を作る別の方法を見てみる。  
例えば人物を表すデータ型を作り、その人物を表すために名前, 苗字, 年齢, 身長, 電話番号, 好きなアイスクリームの味を記録する時を考える(アイスクリームは重要)。  
そんな時は先ほど紹介したデータ型の作り方では以下のように記述できる。

```
data Person = Person String String Int Float String String deriving (Show)
```

そしてPerson型の値から特定の情報(例えば好きなアイスクリームの味等)を取り出すには別個関数を用意する必要がある。

```
firstName :: Person -> String
firstName (Person firstname _ _ _ _ _) = firstname

lastName :: Person -> String
lastName (Person _ lastname _ _ _ _) = lastname

flavor :: Person -> String
flavor (Person _ _ _ _ _ flavor) = flavor
```

これで動くことは動くがかなり冗長でいけてない。  
そこで**レコード構文**と呼ばれる構文で同じ機能を実現できる。以下はレコード構文でPersonを表した例。

```
data Person = Person { firstName :: String
                     , lastName :: String
                     , age :: Int
                     , height :: Float
                     , phoneNumber :: String
                     , flavor :: String } deriving (Show)
```

このレコード構文を使うことで`firstName`, `lastName`等の関数を自動的に作成してくれる。  

```
*Main> :t firstName
firstName :: Person -> String
*Main> :t flavor
flavor :: Person -> String
```

レコード構文を使う時と使わない時の分け方はそれぞれどこに何のフィールがあるかが自明でない時と自明である時の違いになる。  
例えば今のPersonの場合は、phoneNumberが何番目に来るかわからないから定義する際は順番は問われない(全てのフィールドを定義する必要はあるが)。  

それに対して三次元Vectorのデータ型を作ることを考えた場合、三次元Vectorは以下のように定義できる。

```
data Vector = Vector Float Float Float deriving (Show)
```

三次元Vectorの引数はFloat型が三つと定義されているが、これはそれぞれx方向, y方向, z方向の値を取ると容易に想像がつく。そのためこのケースではレコード構文を使う必要がない。

### 型引数
値コンストラクタとは引数をとって新しい値を生み出すものである。それに対して型コンストラクタは型を引数にとって型を返すものである。  
Maybe a型を例に考えてみる。  
```
data Maybe a = Nothing | Just a
```
この左辺の`a`が型引数である。単なるMaybeという型の値は存在できない。aは多層型であり、IntやStringどのような型も取り得ることができる(Maybe a型も取ることができる)。

#### Maybe a型？
Maybe a型は失敗するかもしれない計算に使う型。  
Maybe単体では型になりえないので`Maybe+何かしらの型`で初めて一つの型になる。  
計算に失敗した場合は`Nothing`を返し、成功した場合は`Just 10`, `Just "hoge"`等のMaybeに続く型に応じた値がJustに包まれて返る。

#### リスト型
実はMaybe a型以外にも型引数を取る型を今まで扱っていた。それはリスト型。
リスト型単体では`[]`であり、型引数をとったリスト型は`[a]`となる。

#### 三次元ベクトル
三次元ベクトルの型で型引数の例を挙げてみる。
```
data Vector a = Vector a a a deriving (Show)
```
このように型引数`a`を取ることでこの三次元ベクトルは多層型を実現している。  
この三次元ベクトル型を引数に受け取る関数を以下に定義する。  
```
// ベクトルの加算
vplus :: (Num a) => Vector a -> Vector a -> Vector a
(Vector i j k) `vplus` (Vector l m n) = Vector (i+l) (j+m) (k+n)

// ベクトルの内積
dotProd :: (Num a) => Vector a -> Vector a -> a
(Vector i j k) `dotProd` (Vector l m n) = (i*l) + (j*m) + (k*n)

// ベクトルをスカラー倍する
vmult :: (Num a) => Vector a -> a -> Vector a
(Vector i j k) `vmult` m = Vector (i*m) (j*m) (k*m)
```

### インスタンスの自動導出
Haskellは特定の型クラスのインスタンス宣言を自動導出するする機能を備えている。  

例えば何か自分で定義した型が等値性(`=`や`/=`での比較)を持っていたいのであればその型はEq型クラスのインスタンスである必要がある。  
そういう時に`deriving`キーワードを使えばHaskellがEq型クラスの振る舞いを自動導出してくれる。

例えば先ほどのVectorを例にとってみる。
```
data Vector a = Vector a a a deriving (Eq)
```
このようにderivingでEq型クラスのインスタンスとしての自動導出をしてもらうことでVector型はEq型クラスのインスタンスとしての振る舞いをすることができる(つまり等値計算ができるようになる)。  

#### 色々な型クラスのインスタンスの例
例えば以下のようにDay型を定義する。
```
data Day = Monday | Tuesday | Wednesday |
           Thursday | Friday | Saturday | Sunday
           deriving(Eq, Ord, Show, Bounded, Enum)
```
こうすることでEq, Ord, Show, Bounded, Enum型のインスタンスとしての振る舞いをすることができるようになる。

### 型シノニム
[Char]とStringは同値である。これは**型シノニム**を用いて実装されている。  
型シノニムそのものはと行くに何もしない。ある型に別の名前を与えて、コードを読みやすくするためのものである。
```
type String = [Char]
```

#### そこを左に行って、すぐ右へ
型引数を２つ取るデータ型として`Either`を紹介する。  
以下はEitherの定義。
```
data Either a b = Left a | Right b deriving (Eq, Ord, Read, Show)
```

Eigherには値コンストラクタが２つあり、`Left`は型引数aをとり`Right`は型引数bをとる。  
このEither型を使うことで「２つの型のうちどちらか一方」という値を表すことができる。
```
*Main> :t Left True
Left True :: Either Bool b
```
上記の例をみてみると`Left True`とした場合の型は`Either Bool b`となっていることがわかる。
これは`Either a b`という型を考えた時にLeft値コンストラクタはa型の型引数をとっており(この例だとBool型)、Right値コンストラクタは特に使ってないので多層型のbを取るということになる。
```
*Main> :t Right True
Right True :: Either a Bool
```
逆にRightを使った時はRightは型引数b(この例だとBool型)をとり、Leftは多層型のaということになる。

#### MaybeとEitherを使った例
```
// 階乗を計算。引数がマイナスの場合はNothingを返す。
maybeFactorial :: Int -> Maybe Int
maybeFactorial n
  | n < 0     = Nothing
  | n <= 1    = Just n
  | otherwise = (*n) <$> maybeFactorial (n-1)

// 階乗を計算。引数がマイナスの場合はString(エラーメッセージ)を返す
eitherFactrorial :: Int -> Either String Int
eitherFactrorial n
  | n < 0     = Left "can not take negative number"
  | n <= 1    = Right n
  | otherwise = (*n) <$> eitherFactrorial (n-1)
```
主にMaybeとEitherの違いはMaybeは失敗したらNothingを返すだけなことに対して、Eitherだと失敗したらString(エラー内容)、成功したら値を返す等の処理を行うことができる。

### 再帰的なデータ構造
今までみてきたように代数データ型の値コンストラクタは複数のフィールドを持ったり、フィールドを持たないこともできる。  
フィールドの型は具体型であれば良いため、自分自身の型をフィールドに持つこともできる。  
例えばList型を再帰的なデータ構造で定義してみる。
```
data List a = Empty | List a (List a) deriving (Eq, Ord, Show, Read)
```
これは値コンストラクタにはEmptyとListがあり、Listは多層型のaと`List a`型の値をとることを示している。はじめは混乱するかもしれないが値コンストラクタが引数にとっている`List a`は型である。値コンストラクタではない。  
以下は使用例。
```
*Main> List 'a' Empty
List 'a' Empty
*Main> List 'a' (List 'b' Empty)
List 'a' (List 'b' Empty)

// 中置記法
*Main> 1 `List` (2 `List` (3 `List` Empty))
List 1 (List 2 (List 3 Empty))
```
まずは`List 'a' Empty`を例にとって考えてみる。これは`[]`を使って表現すると`'a':[]`と同じ意味となる。  
同じように考えると`List 'a' (List 'b' Empty)`は`'a':('b':[])`となる。  

中置記法で表現された``1 `List` (2 `List` (3 `List` Empty))``ついても同様に`1:(2:(3:[]))`と表現できる。 型によるのかもしれないが再帰的なデータ構造を表現する場合は中置記法の方がわかりやすいかもしれない。

#### 木を植えよう
他に再帰的なデータ構造の例として二分木を作ってみる。以下は定義。
```
data Tree a = EmptyNode | Node a (Tree a) (Tree a) deriving (Eq, Ord, Show, Read)
```

要素の挿入と要素が存在するかの確認の定義(ついでにEmptyの場合のTreeの作成)
```
singleton :: a -> Tree a
singleton x = Node x EmptyNode EmptyNode

insertNode :: (Ord a) => a -> Tree a -> Tree a
insertNode x EmptyNode = singleton x
insertNode x (Node a left right)
  | x == a = Node x left right
  | x < a  = Node a (insertNode x left) right
  | x > a  = Node a left (insertNode x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyNode = False
treeElem x (Node a left right)
  | x == a = True
  | x < a  = treeElem x left
  | x > a  = treeElem x right
```

### 型クラス中級講座
ここで型クラスについての復習を少しだけ。  
型クラスは特定の振る舞いを定義する。そして定義された通りに振る舞うことのできる型はその型クラスのインスタンスになれる。  
ある型Tがある型クラスCのインスタンスであるとは、型クラスCが定義する関数達を型Tに対して使えるということである。

例えばEq型クラスは等値生判定の振る舞いを定義している(==や/=)。  
そしてInt型はEq型クラスのインスタンスであるため、Int型の値である`4`や`5`は等値比較をすることができる。

#### Eq型クラスの内部
以下はEq型クラスの定義である。
```
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```
`class Eq a where`はEqというなの新しい型クラスの定義が始まることを意味する。  
aは型変数で、将来Eqのインスタンスとなるであろう型を表している。  

そして関数がいくつか定義されている(`==`と`/=`)。これは実体ではないく型宣言だけでも良い。  
ここでは実体も相互再帰という形で実装されている。これの意味は後ほど説明する。

#### 交通信号データ型
実際に例を通してEq型クラスのインスタンスを作ってみる。以下にTrafficLight型を定義する。例を示したいのでEq型クラスの自動導出は行わない。
```
data TrafficLight = Red | Yellow | Green
```
そしてTrafficLightをEq型クラスのインスタンスにする。
```
instance Show TrafficLight where
  Red == Red = True
  Yellow == Yellow = True
  Green == Green = True
  _ == _ = False
```
`instance`キーワードでインスタンスを作成することができる。
ここで`class Eq a where`と`instance Eq TrafficLight where`を比較すると、将来型変数aが何かしらの型に置き換わるという意味がわかる。ここでは実際にTrafficLight型がaに置き換わっている。

クラスを宣言した際に、相互再帰により`==`を定義するために`/=`を使い`/=`を定義するために`==`を使った。  
これによってインスタンス宣言ではどちらか一方だけを上書きすれば良いことになる。  
TrafficLight型の例では`==`が定義されているため、自動的に`/=`も定義されることになる。  
これを型クラスの**最小完全定義(minimal complete definition)** という(重要)。  

もしEqの定義の際に以下のように`==`と`/=`は型宣言で済ませていたらTrafficLightをEqインスタンス宣言する際に`==`と`/=`の両方共実装する必要があった。
```
class Eq a where
  (==) :: a -> a -> Book
  (/=) :: a -> a -> Book
```
同じようにEqだけでなくShowのインスタンスにもしてみる。
```
instance Eq TrafficLight where
  show Red = "Red light"
  show Yellow = "Yellow light"
  show Green = "Green light"
```
`deriving`でShowの自動導出すればそのままRedならRedと出力されるが、このように定義するとRedと入力しても`Red light`と出力されるようになる。

#### サブクラス化
別の型クラスのサブクラスである型クラスを作ることもできる。  
例えばNumの型クラス宣言は以下のようになっている。
```
class (Eq a) => Num a where
```
これは数値計算をしたいのであれば等値比較くらいできなくてはいけないということを意味している。  
このようなとき、Num型クラスはEq型クラスのサブクラスという。aという型はEqのインスタンスであると推論できるため、a型の値に対して`==`を使うことができるようになる。

#### 多相型を型クラスのインスタンスに
Maybeやリストといった型はどうやって型クラスのインスタンスになるのだろうか？  
MaybeがInt等の普通の型と違うのはMaybeそれ自体は具体型ではないところである。Maybeは型引数を一つとって具体型となる。例えば`Maybe Int`や`Maybe Char`など。  
ここで再びEq型クラスの定義を見てみる。  
```
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
  x == y = not (x /= y)
  x /= y = not (x == y)
```
`a`は具体型として使われていることがわかる。そのためMaybeをEqのインスタンスにする際に`instance Eq Maybe where`として宣言することができない。  
```
// OK
instance Eq Int where

// NG
instance Eq Maybe where
```

ではどのように宣言するか？  
Maybeに型を一つ与えてインスタンス化すれば良い。  
```
// OK
instance Eq (Maybe Int) where

instance Eq (Maybe Char) where

instance Eq (Maybe TrafficLight) where
...
```

しかしこれだと冗長になり過ぎてしまう。そこでHaskellでは型引数を変数として定義することが許されている。
```
// 上記のインスタンス宣言と同じ意味を型変数で定義できる
instance Eq (Maybe a) where
```
ただしこれには一つだけ問題がある。それは`==`の定義をする際に発覚するはず。
```
instance Eq (Maybe a) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
```
問題は`x == y`で等値比較をする際にxとyの型がEq型クラスのインスタンスとは限らないということである。  
そのため、インスタンス宣言の際に型クラス制約を設ける必要が出て来る。  

```
instance (Eq a) => Eq (Maybe a) where
  Just x == Just y = x == y
  Nothing == Nothing = True
  _ == _ = False
```
これで完成！

### YesとNoの型クラス
javascriptなどの弱い型付言語ではifの条件にbool以外の値を使うことができる。  
```
if (0) alert("YES!") else alert("NO!")
```
それに対してHaskellは型に厳密なのでこれは許されない。  
なので新しい型クラスを作る例としてこれに近い振る舞いをする型クラスを作り、いくつかの肩をそのインスタンスにしてみる。

```
// YesNo肩クラスの定義
class YesNo a where
  yesno :: a -> Bool

// YesNo型クラスのインスタンスをいくつか定義する
instance YesNo Int where
  yesno 0 = False
  yesno _ = True

instance YesNo [a] where
  yesno [] = False
  yesno _ = True

instance YesNo (Maybe a) where
  yesno Nothing = False
  yesno (Just _) = True
```
色々遊んでみる
```
*Main> yesno (0 :: Int)
False
*Main> yesno (1 :: Int)
True
*Main> yesno []
False
```

### Functor型クラス
Functor型クラスを紹介する。Functorは全体を写せる(map over)ものの型クラスである。実装を以下に示す。
```
class Functor f where
  fmap :: (a -> b) -> f a -> f b
```
Functorは一つの関数fmapを持っており、デフォルト実装は提供していない。  
この型宣言で出てくるfとは具体型ではなく、一つの型引数をとる型コンストラクタのことになる(軽く復習するとMaybeは型コンストラクタ、Maybe Intは具体型)。  
このことからfmapは  
**「ある型aから別の型bへの関数]と、「ある型aに適用されたファンクター値」を取り、「別の型bに適用されたファンクター値」をとる関数と言える。**  
ここでファンクター値とはファンクター型クラスのインスタンスの型をもつ値のことである。

といってもよくわからないと思うので例を示す。  
fmapの型宣言とmapの型宣言を比較してみる。
```
fmap :: (a -> b) -> f a -> f b

map :: (a -> b) -> [a] -> [b]
```
これはよく似ている。実際`[]`はFunctorのインスタンスであるため`f`と表現することもできる。そう考えるとfmapとmapの型宣言は同じであると考えることができる。  
そしてmapは「ある型から別の型への関数」と「ある型のリスト」を取り、「別の型のリスト」を返す関数であることを考えるとmapはリスト限定で動作するfmapと捉えることができる。  
ここでリストに対するFunctorインスタンス宣言を見てみる。

```
instance Functor [] where
  fmap = map
```
`instance Functor [a] where`となっていないのはfは型コンストラクタであるから。  
リストにとってfmapはただのmapであるため、2つの関数をリストに使った結果はもちろん一致する。
```
*Main> fmap (*2) [1..5]
[2,4,6,8,10]
*Main> map (*2) [1..5]
[2,4,6,8,10]
```

#### MaybeもFunctor
強引な言い方をするとファンクターになれるのは箱のような働きをする型である。  
リストはからだったり、ものがいくつか入っている箱だと見なすことができる。他にもMaybe aも箱のように見なすことができる。例えばJust "hoge"やNothingはそれぞれ"hoge"がJustに入っており、Nothingは空であると見えなくもない。  
Maybeは以下のようにファンクターになっている。  
```
instance Functor Maybe where
  fmap f (Just x) = Just (f x)
  fmap Nothing = Nothing
```

非常にシンプルである。以下は使用例
```
Prelude> fmap (*2) (Just 10)
Just 20
Prelude> fmap (++" fuga") (Just "hoge")
Just "hoge fuga"
```

#### TreeだってFunctorになれる
前に定義した以下のTreeもFunctorになれる。
```
data Tree a = EmptyNode | Node a (Tree a) (Tree a)
```
ファンクター宣言は以下のようにfmapを再帰的に定義すると実現できる。  
```
instance Functor Tree where
  fmap f EmptyNode = EmptyNode
  fmap f (Node a left right) = Node (f a) (fmap f left) (fmap f right)
```

以下使用例。
```
*Main> tree
Node 20 (Node 10 EmptyNode EmptyNode) (Node 40 EmptyNode EmptyNode)
*Main> fmap (*10) tree
Node 200 (Node 100 EmptyNode EmptyNode) (Node 400 EmptyNode EmptyNode)
```

#### EitherはFunctorになれるのか?
Either a bはどうであろうか？  
Functor方クラスは型引数を一つだけとる型コンストラクタを要求しているがEitherは型引数を二つとる。  
Either(というよりも型引数を複数とる型コンストラクタ)の場合は型引数を与えて部分適用することで型引数を一つだけとる状態で渡してあげると良い。
```
instance Functor (Either a) where
  fmap f (Right x) = Right (f x)
  fmap f (Left x) = Left x
```
fmapの定義を見るとLeftの場合は何もしないことがわかる。その理由はEither a bのaとbは(同じ場合もあるが)異なる型であり、例えばEither String IntをfmapでLeftとRightの両方を写すとなってもできない。  
なのでRightだけ写すようにする。そしてこれがEitherを使って失敗するかもしれない計算をする場合、Leftはエラーメッセージ、Rightは計算結果を返す理由となる(考えられるのはFunctorだけでないが)。
