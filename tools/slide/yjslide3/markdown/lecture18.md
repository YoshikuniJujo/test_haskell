第18回 IOモナド
===============

はじめに
--------

多くの言語では以下のような形で入出力を扱う。

* 関数が「評価」されるタイミングで入出力を行い
* 関数の返り値として入力を返す

関数の返り値として入力を返すと参照透過性が破壊される
(参照透過性とは引数が同じならば関数の返り値が常に同じになるということ)。
「評価」のタイミングで入出力を行うと、
「評価」の順がプログラムの意味に影響を与えてしまう。

どうすればいいのだろう?

* 入出力を行う機械という値を作れば良い
* 数値型は対話環境で評価するとその値を表示する
* 文字型も対話環境で評価するとその値を表示する
* 機械型は対話環境で評価するとその動作を行う

機械型を数値型と同じように基本的な型として用意すれば良い。

Machine
-------

### HelloWorld

たとえばMachine型という型があったとする。

Machine型の値としてputHelloとputWorldがあり、
それぞれ"Hello"と"World"を表示するとする。

    putHello :: Machine
    putWorld :: Machine

Machine型の値をつなぐ関数nextがあると

    next :: Machine -> Machine -> Machine

"HelloWorld"と表示する関数は以下のように書ける。

    putHelloWorld :: Machine
    putHelloWorld = putHello `next` putWorld

### 入力の処理

出力についてはMachine型でうまくいく。
「出力」というものは本質的に「これやって」「次にこれやって」の
連続なので。

入力がからむとこれはうまくいかなくなる。
入力値を次の機械にわたす必要が出てくるからだ。

入力値を次の機械にわたす仕組みが必要だ。
ひとつめの機械からふたつめの機械に値を渡す関数を考える。

    (>>>) :: Machine -> Machine -> Machine

以下の機械があるとする。

* getLine: 入力を一行読み、次の機械に渡す機械
* putLine: 渡された値を表示する機械

読みこんだ行を表示する機械は以下のように作れる。

    getLine >>> putLine

### 処理の破綻

しかし、このやりかたには問題がある。
以下の機械があるとする。

* getInt: 入力を読み、数に変換し次の機械に渡す機械

そして次のようにすると

    getInt >>> putLine

putLineは文字列が来ることを期待しているので、
数を渡されると予測出来ない動作をするだろう。

IOMcn
-----

### 型の不一致

Machineの問題点は型の不一致が生じているにもかかわらず、
型チェックで検出されないということである。
静的型付け言語であるHaskellでは、
型の不一致は型チェックの段階で検出したい。

そのためにはMachine型に渡される値と渡す値の型を含めれば良い。
これをIOMcn型とする。

    IOMcn a b

### いろいろな機械の型

今まで出てきた機械の型は以下のようになる。

    putHello, putWorld :: IOMcn () ()
    getLine :: IOMcn () String
    getInt :: IOMcn () Int
    putLine :: IOMcn String ()

これらをつなぐ関数は以下のような型となる。

    (>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c

### 正当なつなぎかた

例えば以下のようなつなぎかたは正しい。

    getLine >>> putLine

それぞれの型は以下のようになる。

    getLine :: IOMcn () String
    putLine :: IOMcn String ()
    (>>>) :: IOMcn () String -> IOMcn String () -> IOMcn () ()

つないだ結果の型は

    getLine >>> putLine :: IOMcn () ()

試してみる。

lectures/lecture18/IOMcn.hsが用意してあるので

    % ghci IOMcn.hs
    *IOMcn> runIOMcn $ getLine >>> putLine
    hello
    hello

1行目のhelloはキーボードからの入力であり、2行目はputLineによる出力である。

入力した文字列を表示している、ということ。
「runIOMcnで機械を動かしている」と考えれば良い。

### 正しくないつなぎかた

しかし、以下のつなぎかたは型の不適合となる。

    getInt >>> putLine

(>>>)の型を再掲する。

    (>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c

(>>>)の型における型変数bが、
getIntからはIntであることを要求され、
putLineからはStringであることを要求される。
結果として型エラーとなる。

つまり、おかしな型の値が機械に渡されることはない。

試してみる。

    *IOMcn> runIOMcn $ getInt >>> putLine

    <interactive>:2:23:
        Couldn't match type `[Char]' `Int'
        Expected type: IOMcn Int ()
          Actual type: IOMcn String ()
        In the second argument of `(>>>)', namely `putLine'
        In the second argument of `($)', namely `getInt >>> putLine'
        In the expression: runIOMcn $ getInt >>> putLine

確かに、型エラーとなる。

### 途中に関数をはさむ

(>>>)を使えば次々と機械をつないでいくことができる。

    m1 >>> m2 >>> m3 >>> m4 >>> ...

途中に普通の関数をはさみたいこともある。
例えば入力された文字列を逆にして表示したい等。

このためには関数を機械に変換する関数が必要になる。

    arr :: (a -> b) -> IOMcn a b

これを使うと入力を逆順にして表示は

    getLine >>> arr reverse >>> putLine

arr reverseは文字列を受け取り、
それを逆順にして次の機械に渡す機械である。

試してみる。

    *IOMcn> runIOMcn $ getLine >>> arr reverse >>> putLine
    hello
    olleh

1行目がキーボードからの入力であり2行目がputLineによる出力である。
入力された文字列が逆順で表示されている。

### 値を機械に渡す

### 機械が返す値を使用する関数

#### 導入

#### 関数message

#### 機械を返す機械

#### 機械app

#### 関数sayHello

#### 関数greeting

### まとめ

IOモナド
--------

### IO型

### 相互変換

### モナド関数

### 機械に値を渡す機械

### 出力の例

### 入力の例

### モナドは一方通行

### Monadクラス

### まとめ

まとめ
------
