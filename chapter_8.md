# すごいHな勉強会
## 第八章 - 入出力
### 不純なものと純粋なものを分離する
Haskellは純粋関数型言語であるため関数が副作用を持つことは許されない。関数が同じ引数で二回呼びだされたら必ず両方とも同じ結果を返す。  
これはものの状態を変更しないということになってしまうが、これだとどうやってHaskellしかしながら例えばモニタに出力することが必要になる時などはどうしても状態を変更する必要が出てくる。  
そのため、Haskellでは副作用をもつ関数を扱うためのシステムを持っている。この章ではそれらのシステムについて紹介する。

### Hello, World!
これまでは作った関数をghciにロードしていた。  
今回初めて直接実行するプログラムを書く。それは**Hello, World!**
```
main = putStrLn "Hello, World!"
```
このコードをghcでコンパイルして実行。
```
[ytanimot@MBP-13UAS-088]$ ghc --make chapter_8
[1 of 1] Compiling Main             ( chapter_8.hs, chapter_8.o )
Linking chapter_8 ...
[ytanimot@MBP-13UAS-088]$ ./chapter_8
Hello, World!
```

何一つとして面白くはないがここで一旦putStrLnについて見てみる。
```
*Main> :t putStrLn
putStrLn :: String -> IO ()
```
これはString型の値をとって**I/Oアクション**を返すという意味である。I/Oアクションとは、実行されると副作用(入力を読んだり画面やファイルに何かを書き出したり)を含む動作をして結果を返す何かのことである。  
文字列を端末に表示するアクションは実際には意味がある返り値ではないためダミーの値として`()`が使われる。

### I/Oアクションどうしをまとめる
複数のI/Oアクションを糊付けして1つにするためにdo構文を使うことができる。
```
main = do
  putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"
```
do構文の最後が`IO ()`型を返す式であれば途中でどの型を返す式が扱われても機能する。  
そして`main = putStrLn "Hello, World!"`の型からもわかる通りmainは`IO ()`型となる。()は何かを意味する。その何かには具体型が入る。  

ちなみにここで`getLine`を見てみる。
```
Prelude> :t getLine
getLine :: IO String
```
getLineは`IO String`型である。これはStringを生成するI/Oアクションという意味になる。ユーザが端末に何か入力する(I/Oアクション)のを待って、それから何かを文字列として返すということになる。  
そして`name <- getLine`だが、これは「getLineがI/Oアクションを実行した結果をnameに束縛する」という意味になる。getLineはI/Oアクションを実行した後にString型の値を返すため、nameにはユーザが入力した値がString型の値として束縛されることになる。

ちなみに以下のコードはNGである。理由は型が合わないから(StringとI/Oアクション)。
```
nameTag = "Hello, my name is " ++ getLine
```
そのため、まずI/OアクションからStringの値を取り出してやる必要がある。そのためには必ず`name <- getLine`のように変数に束縛してからその変数を使うといった手順が必要となる。

ちなみに以下のようにputStrLn関数でfooに束縛することもできる。
```
main = do
  foo <- putStrLn "Hello, what's your name?"
  name <- getLine
  putStrLn $ "Hey " ++ name ++ ", you rock!"
```
ただしこれは何も意味はない、putStrLnは`IO ()`を返す関数なのでI/Oアクション(画面に出力)をした後に()を返している。fooには()が入るため使えない。  

I/Oアクションが実行されるのは、mainという名前を与えられたとき、あるいはdoブロックで作った別の大きなI/Oアクションの中にあるときである。

もう一つI/Oアクションが実行されるのはghciに値を入力してenterを押した時である。  
ghciに値を入力して結果がそのまま出力されているのは内部でshow関数に適用した結果をputStrLnで表示しているからである(print関数の実行)。

#### I/Oアクションの中でletを使う
do構文の中でlet構文を使って純粋な値を変数に束縛できる。  
`<-`がI/Oアクションを実行した結果を変数に束縛するのに対して、letはI/Oアクションの中で普通に変数に値を与えたい時に使う。  

以下に例を示す。
```
main = do
  putStrLn "Hello, what's your first name?"
  firstName <- getLine
  putStrLn "Hello, what's your last name?"
  lastName <- getLine
  let bigFirstName = map toUpper firstName
      bigLastName = map toUpper lastName
  putStrLn $ "Hey, " ++ bigFirstName ++ " "
                     ++ bigLastName ++ "!"
```

#### mainの再帰呼び出し
mainを再帰呼び出しすることもできる。
```
main :: IO ()
main = do
  line <- getLine
  if null line
    then return ()
    else do
      putStrLn line
      main
```
mainはIO ()型の値を返すため、最後のmainは型としても正しい。  
`return () `は簡単に言うと文脈にそった値を返す関数であるが、これは13章で説明するモナドが関わってくるため詳しくは説明を割愛する。  
このプログラムはコマンドラインからの入力をそのまま出力する。空のままEnterが押されたらプログラムを終了する。

### いくつかの便利なI/O関数
#### when
whenはControl.Monadモジュールにある関数である。  
BoolとI/Oアクションを受け取り、Trueなら渡されたI/Oと同じものを返す。(正確にはApplicative値)
```
main = do
  input <- getLine
  when (input == "hoge") $ putStrLn input
```
これはgetLineで得られた値が"hoge"なら"hoge"を出力する(IO ()型の値を返すことになる)。それ以外の場合は`return ()`する。

#### sequence
eI/Oアクションのリストを受け取り、それらを順に実行するI/Oアクションを返す。  
このI/Oアクションが生成する結果は、実行した全てのI/Oアクションの結果からなるリストである。  
```
main = do
  rs <- sequence [getLine, getLine, getLine]
  print rs
```

#### mapM
「リストに対してI/Oアクションを返す関数をマップし、それからシーケンスにする」という操作は頻出するため、mapMとmapM_が用意されている。  
```
*Main> mapM print [1..10]
1
2
3
4
5
6
7
8
9
10
[(),(),(),(),(),(),(),(),(),()]

*Main> mapM_ print [1..10]
1
2
3
4
5
6
7
8
9
10
```

#### forM
forM(Control.Monadにある)はmapMと引数の取り方を逆にした関数。  
I/Oアクションを返す関数を後にとるのでラムダ式でI/Oアクションを返す関数を自分で定義し、その結果をリストにマップするなどの処理がやりやすい。forM_もある。
```
forMSample :: IO ()
forMSample = do
  a <- forM_ [1..5] $ \i -> do
    print i
    return i
  print a
```
