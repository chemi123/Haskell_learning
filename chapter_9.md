# すごいHな勉強会
## 第九章 - もっと入力、もっと出力
### ファイルとストリーム
この章ではHaskellでのストリームの扱い方を見ていく。

#### 入力のリダイレクト
まずはファイルを直接リダイレクトで入力ストリームに流す方法を紹介。
```
import Control.Monad

fileInput :: IO ()
fileInput =
  forever $ do
    l <- getLine
    putStrn l
```
foreverはctrl+C等で中断が入らない限り引数で受けとったI/Oアクションを無限回数実行する関数(正確にはI/Oアクションだけでなく、アプリカティブ型クラスののインスタンス型の値をとる)。  
ファイル読み込みの場合はEOFで終了する。以下は実行例。

```
[ytanimot@MBP-13UAS-088]$ echo "hoge \nfuga \npiyo" > tmp
[ytanimot@MBP-13UAS-088]$ runghc chapter_9.hs < tmp      
hoge
fuga
piyo
chapter_9.hs: <stdin>: hGetLine: end of file  <-- EOFで終了
```

#### 入力ストリームから文字列を得る
もう少し扱いやすい入力ストリーム例としてgetContents関数を使う。  
getContentsは標準入力からEOFに達するまで全ての文字を読み込む。型はIO String型で、getContentsの良いところは遅延評価な部分である。  
まずは例を見てみる。
```
getContentsSample :: IO ()
getContentsSample = do
  l <- getContents
  putStr l
```
実行結果
```
[ytanimot@MBP-13UAS-088]$ runghc chapter_9.hs < tmp
hoge
fuga
piyo
```
getContentsは一度に入力ファイルの全ての内容をメモリにのせて、それを変数に束縛することはしない。  
必要になる度に変数に束縛してから評価する。  
ファイル入力だけではなく端末から入力もできる。
```
[ytanimot@MBP-13UAS-088]$ runghc chapter_9.hs
hoge
hoge
piyo
piyo
fuga
fuga
```
このケースの方が遅延評価がわかりやすい。`l <- getContents`の部分ではlには何も束縛されてない。  
いざ`putStr l`となってlを表示する時になってgetContentsによる評価が始まる。

### ファイルの読み書き
まずはリダイレクトではないファイルの入力を紹介。
```
main :: IO ()
main = do
  handle <- openFile "tmp" ReadMode
  contents <- hGetContents handle
  putStr contents
  hClose handle
```
読めばどのような挙動かが大体わかると思う。  
最初の行はopenFileでtmpファイルを読むことにより得られたhandlerをhandleに束縛している。  
ちなみにopenFileの型宣言は以下になる。
```
*Main> :t openFile
openFile :: FilePath -> IOMode -> IO Handle
```
FilePathは`type FilePath = String`となっているのでただのString型の値(ファイルパス)を入れれば良い。
ReadModeだが、これはIOModeの値コンストラクタであり意味はわかると思う。
```
*Main> :t ReadMode
ReadMode :: IOMode
*Main> :i IOMode
data IOMode = ReadMode | WriteMode | AppendMode | ReadWriteMode
```
`openFile "tmp" ReadMode`とした結果、I/Oアクションをしてtmpファイルの入力ストリームを確保し、そのストリームをHandle型の値としてhandle変数に束縛しているということになる。ここで言うhandleとはストリーム中のポインタ、c言語で言うとoffsetのこと。  
そしてhGetContentsでhandleからI/Oアクションによってhandleを介してファイルの内容を取得し、その結果をStringとしてcontentsに束縛する。ただし遅延評価であるため、contentsが評価される`putStr contents`の時に必要に応じてファイルの内容が取得される。hGetContentsもEOF取得で終了する。  
最後にはhCloseでhandleを閉じる。openFileはhandleを閉じる必要がある。

#### withFile関数を使う
ファイルの内容を扱う他の方法としてwithFileがある。  
```
withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
```

この関数はファイルパスとIOMode、そして「ハンドルを受け取ってI/Oアクションを返す関数」を引数として取り、「受け取ったファイルを開いて何かをしてからハンドルを閉じる」といったI/Oアクションを返す。  
「何かをしている」時にエラー等が起きてもハンドルを閉じてくれるので便利。ラムダ式とよく使う。  
以下は使い方例。

```
withFileSample :: IO ()
withFileSample =
  withFile "tmp" ReadMode $ \handle -> do
    contents <- hGetContents handle
    putStr contents
```
openFileを使ったファイル入力の時と全く同じ処理を行なっている。  
先ほども述べたが、ハンドルを閉じる必要はない。終了時及び何かしらエラーが出て処理を抜ける際にハンドルを閉じてくれる。  
そのため、この場合戻り値の型であるIO ()はハンドルを閉じるというI/Oアクションをしてからダミーの値である()を返して終了する。

#### ブラケットの時間
withFileの例のように「何らかのリソース(例えばファイルのハンドル)を獲得し、それに対して何かを行うが、確実にそのリソースが解放されることを保証する」というシナリオはHaskellに限らずわりとよく登場する。  
HaskellではそのようなシナリオのためにControl.Exceptionモジュールがbracketという関数を用意してくれている。
```
bracket :: IO a -> (a -> IO b) -> (a -> IO c) -> IO c
```
最初の引数はリソースの獲得を行うI/Oアクションである。2番目の引数はリソースを解放する関数である。例外が投げられた場合でも呼ばれる。  
そして3つ目の引数はリソースを受け取り、それを使って何かを行う関数である。ファイルの読み込みやファイルへの書き出しは3番目の引数にあたる。  
bracketを使うことによってwithFile関数を実装することができる。
```
withFile' :: FilePath -> IOMode -> (Handle -> IO r) -> IO r
withFile' file mode f =
  bracket (openFile file mode)
          (\handle -> hClose handle)
          (\handle -> f handle)
```

#### ハンドルを握れ！
hGetContentsなど`h`が先頭につく関数はハンドルに対応したファイル等に入出力の操作を行う。  
これらの作業は頻出するため、ハンドルを操作しなくてもファイルを扱うための便利な関数が用意されている。それがreadFile, writeFile, appendFile。以下は使い方となる。
```
readFileSample :: IO ()
readFileSample = do
  contents <- readFile "tmp"
  putStr contents

writeFileSample :: IO ()
writeFileSample = writeFile "tmp" "hoge\nfuga\npiyo"

appendFileSample :: IO ()
appendFileSample = appendFile "tmp" "hoge\nfuga\npiyo"
```

### コマンドライン引数
ここではHaskellの提供するコマンドライン引数を扱うための方法を紹介する。
コマンドライン引数をとるにはSystem.EnvironmentにあるgetArgs及びgetProgNameを用いる。

```
commandLineArgsSample :: IO ()
commandLineArgsSample = do
  args <- getArgs
  progName <- getProgName
  putStrLn "The arguments:"
  mapM_ putStrLn args
  putStrLn "The program name:"
  putStrLn progName
```

### ランダム性
Haskellで乱数を扱う方法を紹介する。  
System.Randomモジュールの提供するrandom関数を見てみる。
```
random :: (RandomGen g, Random a) => g -> (a, g)
```
RandomGen型クラスはランダム性の源として扱える型を表し、Random型クラスはランダムな値になることができる型を表す。  
例えばBoolはTrueかFalseのどちらかにランダムでなることができるので、Bool型はRandom型クラスのインスタンスである。
RandomGen型クラスのインスタンスとしてStdGen型があり、mkStdGen関数でStdGen型の値を作ることができる。
```
mkStdGen :: Int -> StdGen
```
これを使って早速random関数を試してみる。
```
*Main> random (mkStdGen 1)
(7918028818325808681,545291967 2103410263)
```
タプルを得ることができた。左側はRandom型のインスタンス値で右側はStdGen型の値である。  
Random型クラスのインスタンスは様々な型があるので以下のように指定することができる。

```
*Main> random (mkStdGen 1) :: (Bool, StdGen)
(True,80028 40692)
*Main> random (mkStdGen 1) :: (Int, StdGen)
(7918028818325808681,545291967 2103410263)
*Main> random (mkStdGen 1) :: (Char, StdGen)
('\39335',80028 40692)
```

#### ランダム性とI/O
mkStdGenは毎回同じ値を返すのであまり実用性はない。  
そこでSystem.RandomはgetStdGen及びnewStdGenというIO StdGen型のI/Oアクションを提供している。  
getStdGenはグローバル乱数ジェネレータを返し、newStdGenはグローバル乱数ジェネレータを更新(?)する。  
```
getStdGen :: IO StdGen
newStdGen :: IO StdGen
```
早速使ってみる。
```
// グローバル乱数ジェネレータからStdGen型の値を取得
*Main> g <- getStdGen
*Main> g
1602340227 1
*Main> random g
(8902606974506726854,1743912680 2103410263)

// グローバル乱数ジェネレータを更新(この値もStdGen型なのでrandom関数に使える)
*Main> newStdGen
772586250 2147483398

// グローバル乱数ジェネレータの値が更新されていることがわかる
*Main> getStdGen
1602340228 40692
```
