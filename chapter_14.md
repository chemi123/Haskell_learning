# すごいHな勉強会
## 第十四章 - もうちょっとだけモナド
この章ではもう少しモナドを見ていく。  
mtlパッケージを使うので`ghc-pkg list`で確認してなかったらインストールしよう。

### Writer？ 中の人なんていません！
今までMaybe, リスト, IO等のモナドを見てきた。ここではWriterを紹介する。  
Writerは値が2つあり、一つは計算用でもう一つはログ用である。一連の計算を行っている間にログが付与されていき全てのログが単一のログ値になることが保証される。  

使い方を見る前にWriterを使わないでWriterのような要望がありそうな例を見ていく。  
例えばデバッグ目的等でなにが起きているかを説明する文字列を値にくっつけたいとする。盗賊団の人数を引数により、それが大きな盗賊団であるかどうかを文字列付きで返す関数を用意する。

```
// 定義
isBigGang :: Int -> (Bool, String)
isBigGang x = (x > 9, "Compared gang size to 9")

// 実行
*Main> isBigGang 1
(False,"Compared gang size to 9")
*Main> isBigGang 10
(True,"Compared gang size to 9")
```

9人より大きいかそうでないかがBool値とメッセージ付きで返る関数でそれらしく振舞っていることがわかる。  
ではもし(3, "Smallish gang")のように文脈付きの値を文脈を保持したままisBigGangに食わせたい場合はどうしたらいいのだろうか？文脈付きの値を普通の値をとって文脈付きの値を返す関数に食わせるというのはどこかで聞いたような気がするが。  

13章でapplyMaybeを作った時、Maybe a型の値とa -> Maybe bの関数をとってMaybe bの値を返すということをやった。もう一度実装を以下に示す。

```
applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = Just (f x)
```

このapplyMaybe(>>=のことになる)が文脈付きの値を通常の値をとって文脈付きの値を返す関数に食わせることができるのはパターンマッチによりNothingかJust値かを考慮して処理してくれるためである。Nothingだったら計算が失敗、Just値だったら計算が成功しているという文脈を保持したまま計算を続けることができる。  

では同じようにして文脈を保持したまま(3, "Smallish gang")のような文脈付きの値を通常の値をとって文脈付きの値を返す関数(この場合はisBigGang)に食わせることができる関数applyLogを実装してみる。

```
applyLog :: (a, String) -> (a -> (b, String)) -> (b, String)
applyLog (x, log) f = let (y, newLog) = f x in (y, log ++ newLog)
```

これで古いログに新しく発生したログをくっつけてログを単一のログに落とし込むことができる。色々試してみる。

```
*Main> (3, "smallish gang.") `applyLog` isBigGang
(False,"smallish gang.Compared gang size to 9")
*Main> (30, "A freaking platoon.") `applyLog` isBigGang
(True,"A freaking platoon.Compared gang size to 9")
```

ログを保持したまま文脈付きの値を通常の値をとって文脈付きの値を返す関数に食わせることができた。isBigGangだと値がBool値なのでここで完結してしまうが他の関数に食わせて連続で処理していくこともできる。  

```
*Main> (1, "lonely gang.") `applyLog` \x -> (x+1, "a pair of gang.") `applyLog` \x -> (x+2, "companied bad guys.") `applyLog` \x -> (x+7, if (x+7) > 10 then "became big gang." else "still small gang")
(11,"lonely gang.a pair of gang.companied bad guys.became big gang.")
```

#### モノイドが助けにきたよ
現状のapplyLogは(a, String)型の値をとるようになっているがログは必ずしもStringである必要はない。(++)を使って結合しているのでリストでも良い。  
ということでapplyLogの型宣言は`applyLog :: (a, [c]) -> (a -> (b, [c])) -> (b, [c])`に変更できる。  

ところでリストはモノイド型クラスのインスタンスである。そのため(++)で結合している動作はmappendに置き換えることができる。  
これによってapplyLogはリストを結合して文脈を保持していたがモノイド型クラスのインスタンス型であれば文脈を保持できると解釈できる。Stringからリスト、リストからモノイドと抽象度を二段あげることができたということになる。以下はモノイドを利用したapplyLogの実装。

```
applyMonoid :: (Monoid m) => (a, m) -> (a -> (b, m)) -> (b, m)
applyMonoid (x, monoid) f = let (y, newMonoid) = f x in (y, monoid `mappend` newMonoid)
```

ここまで抽象化することができたとなるともはやログを文脈に保持するとい解釈だけでなく、モノイド値であればなんでも文脈に保持できるという解釈ができるようになる。そのためlog部分の名前をmonoidに変更した。  

この抽象化による効果をSumを使って体験してみる。  
Sumとはmappendすることで持つ値を加算させることができるモノイドであることを以前紹介した。  

まずは以下のように食事の注文をしたら飲み物が料金と一緒に追加されて出てくるプログラムを作る(名前付けはあまりイケてないが)。  

```
import Data.Monoid

type Food = String
type Price = Sum Int

addDrink :: Food -> (Food, Price)
addDrink "beans" = ("milk", Sum 25)
addDrink "jerky" = ("whisky", Sum 99)
addDrink _ = ("beer", Sum 30)
```

豆を食べるならセットで牛乳が25セントで、ジャーキーを食べているならウイスキーが99セントで付いてくる。それ以外ならビールが30セントで付いてくる関数である。  
では早速この関数をapplyMonoidに食わせてみる。

```
*Main> ("beans", Sum 10) `applyMonoid` addDrink
("milk",Sum {getSum = 35})
*Main> ("jerky", Sum 25) `applyMonoid` addDrink
("whisky",Sum {getSum = 124})
("beer",Sum {getSum = 139})
*Main> ("jerky", Sum 10) `applyMonoid` addDrink `applyMonoid` addDrink `applyMonoid` addDrink
("beer",Sum {getSum = 169})
```

10セントの豆を食べている時に追加でドリンクを頼むと25セントで牛乳が来て総額35セントになる。同じように25セントのジャーキーを食べているなら99セントのウイスキーが来て総額124セントとなる。  
それ以外ならビールがくるためビールを飲んでから飲み物を追加で頼んでもビールが来る。ビールが来るたびに30セントが加算されていく。

このようにStringのログを文脈として保持していたものがモノイドまで抽象化することで合計金額までも文脈として保持できるようになったと言える。

#### Writer型
これまでで「モノイドのおまけが付いた値」がいかにもモナド値のように振舞うことができることが分かった。  
ではここでそのような値のMonadインスタンスを見ていく。Control.Monad.WriterモジュールがWriter w a型とそのMonadインスタンス及びWriter w a型を扱うための便利な関数をエクスポートしている。以下はWriterの定義。

```
newtype Writer w a = Writer { runWriter :: (a, w) }
```

型引数wはおまけの値のモノイド値を示している。  
Writer値コンストラクタは公開されておらず、代わりにwriter関数を使う。これはWriter値コンストラクタと同じ機能を持っている。以下はMonadインスタンス定義。

```
instance (Monoid w) => Monad (Writer w) where
  return x = Writer (x, mempty)
  Writer (x, w) >>= f = let (Writer (y, w')) = f x
                        in Writer (y, w `mappend` w')
```

wはMonoid型クラス制約がかかっている。これが重要である。  
returnは最小の文脈に包んで返すものであった。Writerの場合最小の文脈とはまだモノイド値が単位元のときである(例えばリストであれば空のリスト、AnyであればAny Falseなど)。  

そして>>=は文脈付の値(モナド値)を通常の値をとってモナド値を返す関数に食わせるものであった。  
Writerのの場合は通常の値はタプルの左側、そしてログ等の保持しておきたい値は右側にあるモノイド値のことである。  
ここで通常の値を関数に食わせると得られる値としては通常の値及びモノイド値となる。通常の値の方はそのままで、モノイド値の方はすでにあるモノイド値にmappendして新しいモノイド値を作る。そしてそれらをWriterに包んで返す動作をする。

早速使ってみる。

```
*Main> runWriter (return "hoge" :: Writer [Char] String)
("hoge","")
*Main> runWriter (return 10 :: Writer (Sum Int) Int)
(10,Sum {getSum = 0})
*Main> runWriter (return 10 :: Writer (Product Int) Int)
(10,Product {getProduct = 1})

// リストに1ずつインクリメントしてリストに追加していく
*Main> runWriter $ (return 1 :: Writer [Int] Int) >>= \x -> writer (x+1, [x])
(2,[1])
*Main> runWriter $ (return 1 :: Writer [Int] Int) >>= \x -> writer (x+1, [x]) >>= \x -> writer (x+1, [x])
(3,[1,2])

// Sumに同様のことを行う
*Main> runWriter (return 1 :: Writer (Sum Int) Int)
(1,Sum {getSum = 0})
*Main> runWriter $ (return 1 :: Writer (Sum Int) Int) >>= \x -> writer (1, Sum x)
(1,Sum {getSum = 1})
*Main> runWriter $ (return 1 :: Writer (Sum Int) Int) >>= \x -> writer (1, Sum x) >>= \x -> writer (1, Sum x)
(1,Sum {getSum = 2})

// Productで2を掛け合わせていく
*Main> runWriter (return 2 :: Writer (Product Int) Int)
(2,Product {getProduct = 1})
*Main> runWriter $ (return 2 :: Writer (Product Int) Int) >>= \x -> writer (2, Product x)
(2,Product {getProduct = 2})
*Main> runWriter $ (return 2 :: Writer (Product Int) Int) >>= \x -> writer (2, Product x) >>= \x -> writer (2, Product x)
(2,Product {getProduct = 4})
```

WriterはShowインスタンスではないためrunWriterで中身を取り出してやる必要がある。  
returnで引数が最小の文脈に包まれるためよく見るとリストなら[]、SumならSum 0、ProductならProduct 返ことがわかる。
\>>=ではモノイド値がmappendされるため値が保持されたまま演算が連結して行われている。  
ちなみにWriterにはfailの実装がないためdo記法でパターンマッチに失敗するとerrorが呼ばれてプログラムがクラッシュする。

#### Writerをdo記法で使う
Writerももちろんdo記法が使える。  
先ほどのProductをdo記法にすると以下のようになる(使い方も兼ねてtellをこっそり挟んでいるが)。

```
productWriter :: Writer (Product Int) Int
productWriter = do
  x <- (return 2 :: Writer (Product Int) Int)
  y <- writer (2, Product x)
  tell $ Product 2
  z <- writer (2, Product y)
  writer (2, Product z)
```

tellはモノイド値を直接文脈にmappendする関数である。この場合Product 2がmappendされているのでモノイド値に2が掛けられている。  
返り値はm ()で型が合わないのでdo構文の最後では使えない。

```
*Main> runWriter productWriter
(2,Product {getProduct = 16})
```

### Reader? それはあなたです!
ここではReaderを**簡単に**紹介する(あまりよくは理解できていないので)。  
以下がReaderの定義となる。関数を包む。

```
newtype Reader e a = Reader { runReader :: (e -> a) }
```

そして以下がモナドインスタンス定義。

```
instance Monad (Reader e) where
    return a         = Reader $ \e -> a
    (Reader r) >>= f = Reader $ \e -> runReader (f (r e)) e
```

returnは値をとってその値を結果として返す最小限の文脈(関数)を返すという意味になる。  
\>>の実装は暗号すぎて正直あまりよく理解はできていないが実際に使ってみることにする。

```

```
