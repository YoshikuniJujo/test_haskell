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

関数がIOMcnに変換できるということは、
値を機械に流し込むことができるということだ。
たとえば"Hello"を機械に流し込むには以下のようにする。

    arr (const "Hello") >>> putLine

引数を無視し"Hello"を返す関数を機械に変換し、
その機械と機械putLineとをつないだ。

よってputHello, putWorldは以下のように定義できる。

    putHello = arr (const "Hello") >>> putLine
    putWorld = arr (const "World") >>> putLine

試してみる。

    *IOMcn> runIOMcn $ arr (const "Hello") >>> putLine
    Hello
    *IOMcn> runIOMcn $ arr (const "World") >>> putLine
    World

引数を無視して"Hello"を返す関数(const "Hello")を機械に変換し

    arr (const "Hello") :: IOMcn () String

これをputLineにつないでいる。

hello.hsを作って以下を書きこむ。

    import IOMcn

    putHello, putWorld :: IOMcn () ()
    putHello = arr (const "Hello") >>> putLine
    putWorld = arr (const "World") >>> putLine

試してみる。

    *IOMcn> :load hello.hs
    *Main> runIOMcn $ putHello >>> putWorld
    Hello
    World

### 機械が返す値を使用する関数

#### 導入

そのときの時間の秒の値が、偶数のときには"olleh"を返し、
奇数のときには"hello"を返す機械を作る。
今が偶数の秒かどうかを返す機械はあるとする。

    isEven :: IOMcn () Bool

試してみる。

    *Main> runIOMcn isEven
    True
    *Main> runIOMcn isEven
    True
    *Main> runIOMcn isEven
    False

その時によってTrueまたはFalseが表示される。
これは関数の返り値が変化しているのではなく、
機械を動作させた結果、機械が次の機械に渡す値が変化しているということ。

#### 関数message

Bool値によって以下のどちらかの機械を返す関数を作る。

* メッセージを逆順で表示する機械
* メッセージをそのまま表示する機械

その関数をmessageという名前で定義する。

    message :: Bool -> IOMcn String ()
    message True = arr reverse >>> putLine
    message False = putLine

greeting.hsを作り、これを書きこみ、先頭に以下を追加する。

    import IOMcn

試してみる。

    *Main> :load greeting.hs
    *Main> runIOMcn $ arr (const "hello") >>> message False
    hello
    *Main> runIOMcn $ arr (const "hello") >>> message True
    olleh

#### 機械を渡す機械

ここまで見てきたなかで以下の機械と機械を返す関数とがある。

    isEven :: IOMcn () Bool
    message :: Bool -> IOMcn String ()

arrと>>>を使って組み合わせて以下の動作の機械を作りたい。

* 偶数の秒には"hello"を逆順で表示し
* 奇数の秒には"hello"をそのまま表示する

まず、isEvenから渡されるBool値を受け取るにはmessage関数を
機械にする必要がある。

    arr message :: IOMcn Bool (IOMcn String ())

これとisEvenをつなげると

    isEven >>> arr message :: IOMcn () (IOMcn String ())

実際に型を見てみよう。

    *Main> :t message
    message :: Bool -> IOMcn String ()
    *Main> :t arr message
    arr message :: IOMcn Bool (IOMcn String ())
    *Main> :t isEven >>> arr message
    isEven >>> arr message :: IOMcn () (IOMcn String ())

#### 機械app

isEven >>> arr messageの型を見ると

    IOMcn () (IOMcn String ())

機械を渡す機械ができてしまっている。
その渡される機械の型はIOMcn String ()であり、
これを動かすためにはStringを渡す必要がある。
つまり、IOMcn String ()にStringを渡し、作動させる機械が必要になる。

より一般的には、IOMcn a bにaを渡し、作動させる機械があれば良い。

    app :: IOMcn (IOMcn a b, a) b

機械とそれに渡す値のタプルを受け取り、機械を作動させて、
結果として、その機械の渡す値を渡す関数である。

今回の目的はIOMcn String ()にStringを渡すことなので、
(IOMcn String (), String)というタプルをappに渡す必要がある。
よって以下の型の機械が必要になる。

    IOMcn () (IOMcn String (), String)

#### 関数sayHello

使える機械と関数に何があるか整理する。

    isEven :: IOMcn () Bool
    message :: Bool -> IOMcn String ()

これらを組み合わせるための道具には以下のものがある。

    arr :: (a -> b) -> IOMcn a b
    (>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
    app :: IOMcn (IOMcn a b, a) b

以下の型の機械があればappにつなぐことができる。

    IOMcn () (IOMcn String (), String)

さらに、この型の機械は以下の型の関数とisEvenをつなげばできる。

    IOMcn Bool (IOMcn String (), String)

この型の関数を作るには以下の型の関数にarrを適用すれば良い。

    Bool -> (IOMcn String (), String)

messageを使えばこの型の関数は簡単に作れる。

    sayHello :: Bool -> (IOMcn String (), String)
    sayHello b = (message b, "hello")

関数sayHelloをgreeting.hsに書きこむ。

#### 関数greeting

対話環境で最終的に求める機械を組み立ててみよう。

    *Main> :reload
    *Main> :t arr sayHello
    arr sayHello :: IOMcn Bool (IOMcn String (), String)
    *Main> :t isEven >>> arr sayHello
    isEven >>> arr sayHello :: IOMcn () (IOMcn String (), String)
    *Main> :t isEven >>> arr sayHello >>> app
    isEven >>> arr sayHello >>> app :: IOMcn () ()
    *Main> runIOMcn $ isEven >>> arr sayHello >>> app
    hello
    *Main> runIOMcn $ isEven >>> arr sayHello >>> app
    olleh

まとめると以下のようになる。

    sayHello :: Bool -> (IOMcn String (), String)
    arr sayHello :: IOMcn Bool (IOMcn String (), String)
    isEven >>> arr sayHello :: IOMcn () (IOMcn Strint (), String)
    isEven >>> arr sayHello >>> app :: IOMcn () ()

よって、求める関数greetingは

    greeting :: IOMcn () ()
    greeting = isEven >>> arr sayHello >>> app

それぞれの機械を説明すると

* isEven: Boolを渡す機械
* arr sayHello:
    + Boolを受け取り(「文字列を受け取る機械」と文字列)を渡す機械
* app: (「文字列を受け取る機械M」と文字列S)を受け取り
    + 機械Mに文字列Sを渡す機械

関数greetingの定義をgreeting.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> runIOMcn greeting
    hello
    *Main> runIOMcn greeting
    hello
    *Main> runIOMcn greeting
    olleh

### まとめ

多くの言語ではIOは以下のように行われる。

* 関数の評価のタイミングで入出力動作を行い
* 入力値は関数の返り値として受け取れる

参照透過性と遅延評価の面からHaskellでは上記の方法は望ましくない。
むしろIOを行う機械を組み立てていくことを考える。
機械が受け取る値の型と渡す値の型を指定すると、
型エラーを検出することができる。

機械や関数を組み合わせるうえで、必要な関数や機械を用意する。
必要な型や機械、関数は以下のようになる。

    IOMcn a b
    arr :: (a -> b) -> IOMcn a b
    (>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
    app :: IOMcn (IOMcn a b, a) b

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
