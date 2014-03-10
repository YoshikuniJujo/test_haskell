第16回 モナド
=============

はじめに
--------

(a -> m b)の形をとるいろいろな関数が存在する。
この形の関数の多くは(a -> m b)と(b -> m c)をつないで、
(a -> m c)を導出できると便利なことが多い。

例としてMaybeについて考えてみる。
小文字のみを文字コードにする関数と偶数のみを2で割る関数があるとする。

    lowerToCode :: Char -> Maybe Int
    evenDiv2 :: Int -> Maybe Int

これらをつないで「小文字の文字コードで偶数のものの半分の値」を返す関数を
作る。

    lowerToCodeDiv2 :: Char -> Maybe Int
    lowerToCodeDiv2 = [lowerToCodeとevenDiv2をつないだもの]

新たに必要な構文
----------------

### let ... in ...

let [定義] in [表現]という形の構文がある。
[定義]中で定義された変数は[表現]のなかで使うことができる。
全体としては[表現]によって表される値となる。

試してみる。

    % ghci
    Prelude> let x = 8 in x * x
    64
    Prelude> x

    <interactive>:3:1: Not in scope: `x'

let内で定義された変数xはin内で使えることがわかる。
また、その変数はこの構文外の環境では使えない。

Maybeをつなげる
---------------

### 導入

#### 小文字の文字コードの半分

「小文字の文字コードで偶数のものの半分の値を返す関数」を
実際に作ってみる。
lectures/lecture16/maybe.hsを作成し編集しよう。

Data.CharのisLower, ordを使うので

    import Data.Char (isLower, ord)

小文字のみ文字コードにする関数

    lowerToCode :: Char -> Maybe Int
    lowerToCode c
        | isLower c = Just $ ord c
        | otherwise = Nothing

maybe.hsに書きこむ。

偶数のみを2で割る関数

    evenDiv2 :: Int -> Maybe Int
    evenDiv2 n
        | even n = Just $ n `div` 2
        | otherwise = Nothing

maybe.hsに書きこむ。

この2つの関数をつなげる。

    lowerToCode :: Char -> Maybe Int
    evenDiv2 :: Int -> Maybe Int

lowerToCodeが返すIntをevenDiv2の引数にする。

    lowerToCodeDiv2 :: Char -> Maybe Int
    lowerToCodeDiv2 c = case lowerToCode c of
        Just n -> evenDiv2 n
        Nothing -> Nothing

maybe.hsに書きこみ、試してみる。

    % ghci maybe.hs
    *Main> lowerToCodeDiv2 'n'
    Just 55
    *Main> lowerToCodeDiv2 'm'
    Nothing

#### 小文字の文字コードの4分の1

今度は「4で割り切れるもののみを4で割る」ようにする。
以下の3つをつなげば良い。

    lowerToCode :: Char -> Maybe Int
    evenDiv2 :: Int -> Maybe Int
    evenDiv2 :: Int -> Maybe Int

定義は以下のようになる。

    lowerToCodeDiv4 :: Char -> Maybe Int
    lowerToCodeDiv4 c = case lowerToCode c of
        Just n -> case evenDiv2 n of
            Just n' -> evenDiv2 n'
            Nothing -> Nothing
        Nothing -> Nothing

#### どのように「つないだ」か

どのように「つないだ」かを考える。

    f :: a -> Maybe b
    g :: b -> Maybe c

関数fの結果がJust xならばxの値にgを適用する。
その結果はJust yまたはNothingとなる。

関数fの結果がNothingなら結果もNothingとなる。

### pipeM

Maybe型を返す関数をつなぐpipeMを書く。

    pipeM :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
    f 'pipeM` g = \v -> case f v of
        Just x -> g x
        Nothing -> Nothing

maybe.hsに書きこむ。

pipeMを使うとlowerToCodeDiv4は以下のように書ける。

    lowerToCodeDiv4 :: Char -> Maybe Int
    lowerToCodeDivr = lowerToCode `pipeM` evenDiv2 `pipeM` evenDiv2

maybe.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> lowerToCodeDiv4 'n'
    Nothing
    *Main> lowerToCodeDiv4 'p'
    Just 28

### arrM

2で割ったうえに3をかけることを考える。
かけ算はとくに割り切れない等がないので

    mul3 :: Int -> Int
    mul3 = (* 3)

これを、pipeMでつなげたい。
pipeMでつなぐには以下の形にする。

    Int -> Maybe Int

この変換を行う関数を作る。

    arrM :: (a -> b) -> (a -> Maybe b)
    arrM f = \x -> Just $ f x

mul3とarrMをmaybe.hsに書きこむ。

小文字のコードを2で割ったうえに3をかける関数を書く。

    lowerToCodeDiv2Mul3 :: Char -> Maybe Int
    lowerToCodeDiv2Mul3 =
        lowerToCode `pipeM` evenDiv2 `pipeM` arrM mul3

maybe.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> lowerToCodeDiv2Mul3 'n'
    Just 165

### bindMとretM

#### bindMとretMの定義

(a -> Maybe b)型の関数をつなぐために用意した関数

    pipeM :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
    arrM :: (a -> b) -> (a -> Maybe b)

前回の講義を思い出してみよう。
引数の型と結果の型の両方に'a ->'があるので、
それらを消すことができる。

    bindM :: Maybe b -> (b -> Maybe c) -> Maybe c
    retM :: b -> Maybe b

こちらのセットを定義する。

    bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
    bindM (Just x) f = f x
    bindM Nothing _ = Nothing

    retM :: a -> Maybe a
    retM = Just

maybe.hsに書きこむ。

#### lowerToCodeDiv2Mul3'の定義

##### 変数を使わない定義

bindMとretMを使ってlowerToCodeDiv2Mul3'を定義する。

    lowerToCodeDiv2Mul3' :: Char -> Maybe Int
    lowerToCodeDiv2Mul3' c =
        lowerToCode c `bindM` evenDiv2 `bindM` (retM . mul3)

##### 変数を使う定義

同じことを以下のように書くこともできる。

    lowerToCodeDiv2Mul3' :: Char -> Maybe Int
    lowerToCodeDiv2Mul3' c =
        lowerToCode c `bindM` \n ->
        evenDiv2 n `bindM` \n' ->
        retM $ mul3 n'

この形は「lowerToCode cの結果にnを束縛し、
evenDiv2 nの結果にn'を束縛し、
mul3 n'の値を返す」と読むことができる。

どちらかの定義をmaybe.hsに書きこむ。

##### 試してみる

試してみる。

    *Main> :reload
    *Main> lowerToCodeDiv2Mul3' 'p'
    Just 168

##### 変数を使った定義の解説

変数を使った定義を見てみよう。

    lowerToCodeDiv2Mul3' c =
        lowerToCode c `bindM` \n ->
        evenDiv2 n `bindM` \n' ->
        retM $ mul3 n'

これに適切な括弧をつけると以下のようになる。

    lowerToCode c `bindM` (\n ->
        evenDiv2 n `bindM` (\n' ->
            retM $ mul3 n'))

### まとめ

以下の型を持つ「失敗するかもしれない計算」がある。

    a -> Maybe b

そういった計算を「つなぐ」以下の型を持つ関数をつくる。

    (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)

このような関数を使うときれいな抽象化が実現できる。

普通の計算も同様の形に直すことで、上記の関数で「つなぐ」ことができる。
そのための変換関数は以下の形となる。

    (a -> b) -> (a -> Maybe b)

これらの関数はより単純な以下のような関数とのあいだで相互に変換ができる。

    bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
    retM :: a -> Maybe a

Stateをつなげる
---------------

### 導入

メモリ機能付きの電卓について考えてみる。
以下の計算を例として見ていこう。

    (3 * 4 + 2 * 5) * 7

以下の順にボタンを押せば良い。

    3 * 4 M+ C 2 * 5 M+ c MR * 7

以下のような手順で計算をしている。

* 3 * 4を計算しメモリに足す
* 表示をクリアする
* 2 * 5を計算しメモリに足す
* 表示をクリアする
* メモリを呼び出す
* 7をかける

それぞれの操作は、

* 画面の表示とメモリの状態を引数としてとり
* 画面の表示とメモリの状態を返り値として返す

### 必要な関数とその型

#### M+とMR

今回の例では実際の電卓とは異なるが

* M+は画面のクリアもすることとする
* MRをする前には画面がクリアされている必要がある

とする。「M+は画面の状態を返さず、
MRは画面の状態を受けとらない」ということ。

よって、それぞれの型は以下のようになる。

    mplus :: Int -> Int -> ((), Int)
    mrecall :: () -> Int -> (Int, Int)

表示を受け取らないということを()で表現している。

#### 画面の表示を変化させる関数

画面の表示を変化させる関数を作る。
画面はIntで表現されるので(Int -> Int)関数を変換すれば良い。
このとき、メモリは変化させる必要がない。
つまり、メモリの値は引数として取り、そのまま返り値とする。

この関数をarrCとするとその型は

    arrC :: (Int -> Int) -> Int -> Int -> (Int, Int)

この関数は(+ 3)や(* 2)等の単純な演算を電卓の画面を変化させる関数に変換する。

ただの数字キーは引数を取らないでIntを返す関数と考えることができる。

    () -> Int

これに対応するためにはarrCは以下の型である必要がある。

    arrC :: (() -> Int) -> () -> Int -> (Int, Int)

よってarrCは型変数を使って以下のような型として作ることにする。

    arrC :: (a -> Int) -> a -> Int -> (Int, Int)

次にarrCを画面のクリアにも使うことを考える。
画面をクリアするということは画面の値をユニット値()にするということ。
つまり値をクリアする関数の型はInt -> ()と考えることができる。

よって値をクリアする関数を電卓の画面をクリアする関数に変換する関数の型は

    arrC :: (Int -> ()) -> Int -> Int -> ((), Int)

前で定義したarrCの型と合わせて考えると、
型変数を2つ使って以下のようになる。

    arrC :: (a -> b) -> a -> Int -> (b, Int)

つまり値をa型からb型の値に変化させる関数を引数としてとって、
電卓の表示をa型からb型の値に変化させる関数を返す関数、ということ。

#### まとめ

ここまでで、必要な関数は以下の3つとなる。

    mplus :: Int -> Int -> ((), Int)
    mrecall :: () -> Int -> (Int, Int)
    arrC :: (a -> b) -> a -> Int -> (b, Int)

### mplusとmrecall

#### mplus

mplusを定義してみよう。

* 画面の表示とメモリの内容を足してメモリに保存する
* 画面の表示はクリアする

よって以下のようになる。

    mplus x m = ((), x + m)

calc.hsを作成しこれを書きこむ。

    mplus :: Int -> Int -> ((), Int)
    mplus x m = ((), x + m)

#### mrecall

mrecallを定義する。

* メモリの内容を画面に呼び出す
* 呼び出した直後はメモリと画面は同じ値となる
* mrecallを呼ぶ前には画面がクリアされている必要がある

    mrecall :: () -> Int -> (Int, Int)
    mrecall _ m = (m, m)

これをcalc.hsに書きこむ。

#### 試してみる

##### mplus

    *Main> :load calc.hs
    *Main> mplus 3 0
    ((), 3)
    *Main> mplus 4 8
    ((), 12)

画面の表示が3でメモリが0のときmplusをすると、
画面はクリアされ(ユニット値())され、メモリは3となる。

画面の表示が4でメモリが8のときmplusをすると、
画面はクリアされ、メモリは12となる。

##### mrecall

    *Main> mrecall () 4
    (4, 4)
    *Main> mrecall () 19
    (19, 19)

mrecallをする前には画面がクリアされている必要がある。
つまり、画面はユニット値である。

mrecallをするとメモリの内容が画面に呼び出される。
つまり、mrecallの直後はメモリの内容と画面は同じ値になる。

### arrC

#### 定義

次に必要なのは関数によって電卓の表示を変化させる関数である。

    arrC :: (a -> b) -> a -> Int -> (b, Int)

メモリの値は変化させない。
つまり、引数として取りそのまま返す。
画面の値は与えられた関数で変化させる。

よって、以下のような定義となる。

    arrC f x m = (f x, m)

これをcalc.hsに書きこむ。

#### 試してみる

##### 数字キーを押した場合について

まずは数字キーを押した場合について試してみる。
数字キーは画面の値を無視して数を返す関数で表されるので
const [数字]とすれば良い。

    *Main> :reload
    *Main> arrC (const 8) () 4
    (8, 4)
    *Main> arrC (const 11) () 32
    (11, 32)

メモリの値は変化させずに表示が与えられた数となる。
数字を入れる前に表示がクリアされている点に注意する。

##### 演算

画面に数字が表示されている状態で、それに対する演算を行ってみる。

    *Main> arrC (* 3) 2 5
    (6, 5)
    *Main> arrC (+ 8) 7 23
    (15, 23)

メモリの値は変化しない。画面の値に与えられた演算が行われている。

##### 整数以外を返す演算

arrCは十分に一般的に作ったので四則演算以外も可能である。

    *Main> arrC even 4 7
    (True,7)
    *Main> :m + Data.Char
    *Main Data.Char> arrC chr 99 37
    ('c',37)

電卓のクリアキーは画面の値を無視して、画面の値をクリアするので、

    *Main Data.Char> arrC (const ()) 37 8
    ((), 8)

### Calc型

今まで扱ってきた関数は共通の形を持っている。
その共通部分を取り出すと以下のようになる。

    a -> Int -> (b, Int)

それぞれの型は以下のような意味を持つ。

* aは直前の画面の値
* 1つめのIntは直前のメモリの値
* bは直後の画面の値
* 2つめのIntは直後のメモリの値

今後、この型を頻繁に用いるので別名をつけておく。

    type Calc a b = a -> Int -> (b, Int)

たとえば以下のようになる。

    mplus :: Calc Int ()
    mrecall :: Calc () Int
    arrC even :: Calc Int Bool
    arrC chr :: Calc Int Char

### pipeC

計算の部品はそろったので次はそれらを組み合わせる。
組み合わせるための関数の型は以下の形となるはずだ。

    pipeC :: Calc a b -> Calc b c -> Calc a c

画面の値をaからbにする計算と、
画面の値をbからcにする計算とをつないで、
画面の値をaからcにする計算をつくる。

以下のような定義になるだろう。

    f `pipeC` g = \x m -> let (x', m') = f x m in g x' m

let X in Yの形でXのなかで束縛した変数をYのなかで使える。

pipeCの定義は以下のようになっている。

はじめの画面の値xとメモリの値mをfに与え、
その結果をx', m'に束縛する。
そのx', m'をgに与え、その結果を返す。

Calc型の定義と合わせてcalc.hsに書きこむ。

    type Calc a b = a -> Int -> (b, Int)

    pipeC :: Calc a b -> Calc b c -> Calc a c
    f `pipeC` g = \x m -> let (x', m') = f x m in g x' m'

試してみる。

    *Main> :reload
    *Main> (arrC (const 3) `pipeC` arrC (* 2)) () 23
    (6, 23)
    *Main> (arrC (const 4) `pipeC` mplus) () 3
    ((), 7)

1つ目の例は、
数字キー3と、(* 2)という演算とをつなぎ、
画面はクリアでメモリは23の状態に適用している。

2つ目の例は、
数字キー4と、M+という演算をつなぎ、
画面はクリアでメモリは3の状態に適用している。

### 計算例

最初に挙げた例は以下の通り。

    (3 * 4 + 2 * 5) * 7

これをここまで見てきた枠組みで作ってみよう。

    example :: Calc () Int
    example =
        arrC (const 3) `pipeC`
        arrC (* 4) `pipeC`
        mplus `pipeC`
        arrC (const 2) `pipeC`
        arrC (* 5) `pipeC`
        mplus `pipeC`
        mrecall `pipeC`
        arrC (* 7)

これをcalc.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> example () 0
    (154,22)

初期状態は、画面はクリアされていて、メモリは0であるので、
それを引数として与えている。

### State型

### bindCとretC

### 計算例

#### 変数を使わない定義

#### 変数を使う定義

### まとめ

MaybeとStateの比較
------------------

モナド
------

Monadクラス
-----------

Maybeモナド
-----------

Stateモナド
-----------

まとめ
------
