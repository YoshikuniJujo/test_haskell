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

Calc型を見てみる。

    type Calc a b = a -> Int -> (b, Int)

State型を作ってみる。以下のようになる。

    type State b = Int -> (b, Int)
    type Calc a b = a -> State b

a -> bとa -> State bとを比較することで以下のことがわかる。

* a -> bは画面の値の変化である
* a -> State bは画面とメモリの値の変化である
* Stateは画面の値の変化にメモリの値の変化を追加する

### bindCとretC

#### bindC

pipeCの型をState型を使って書き換えてみる。

    pipeC :: (a -> State b) -> (b -> State c) -> (a -> State c)

画面とメモリを変化させる関数をつないでいる。
画面の変化については明示的に示されているが、
メモリの変化についてはState型の内部に隠されている。

pipeCを見ると1つめと2つめのa型は
「引数と結果に同じ型変数が存在する」ので消すことができる。

    bindC :: State b -> (b -> State c) -> State c

bindCの定義は以下のようになる。

    bindC f g = \m -> let (x, m') = f m in g x m'

fに状態mを与え結果の値と状態をgに与えている。

#### retC

同様にarrCも以下のようにできる。

    arrC :: (a -> b) -> (a -> State b)

引数と返り値の両方にある型変数aを消して

    retC :: b -> State b
    retC x = \m -> (x, m)

#### まとめ

これらをcalc.hsに書きこむ。

    type State a = Int -> (a, Int)

    bindC :: State a -> (a -> State b) -> State b
    bindC f g = \m -> let (x, m') = f m in f x m

    retC :: a -> State a
    retC x = \m -> (x, m)

### 計算例

#### 変数を使わない定義

最初の例は以下の通りである。

    (3 * 4 + 2 * 5) * 7

これをbindC, retCで書いてみる。

    example' :: State Int
    example' =
        retC 3 `bindC`
	(retC . (* 4)) `bindC`
	mplus `bindC`
	const (retC 2) `bindC`
	(retC . (* 5)) `bindC`
	mplus `bindC`
	mrecall `bindC`
	(retC . (* 7())

これをcalc.hsに書きこみ、試してみる。

    *Main> :reload
    *Main> example' 0
    (154,22)

#### 変数を使う定義

同じことを以下のように書くこともできる。

    example'' =
        retC 3 `bindC` \x ->
        retC (x * 4) `bindC` \y ->
        mplus y `bindC` \_ ->
        retC 2 `bindC` \z ->
        retC (z * 5) `bindC` \w ->
        mplus w `bindC` \_ ->
        mrecall () `bindC` \v ->
        retC (v * 7)

これは以下のように読むことができる。

* retC 3で返る値でxを束縛し
* retC (x * 4)で返る値でyを束縛し
* mplus yでyの値を状態に足し、返り値は捨て
* retC 2で返る値でzを束縛し
* retC (z * 5)で返る値でwを束縛し
* mplus wでwの値を状態に足し、返り値は捨て
* mrecall ()で状態の値を呼び出し、vを束縛し
* retC (v * 7)の値を返す

ちなみに、「[値]が[変数]を束縛する」や「[値]で[変数]を束縛する」は
「[変数]に[値]を代入する」とほぼ同じ意味と考えて良い。

「束縛する」は「代入する」をより関数的に表現していると考えよう。

### まとめ

メモリ付き電卓の例を見た。

関面の値とメモリの値のペアを次々と変換していく。
State型を作ることで、画面の値にだけ注目し、
メモリの値の変化を隠すことができた。

以下の関数でそれぞれの変換を部品としてつないでいくことができる。

    pipeC :: (a -> State b) -> (b -> State c) -> (a -> State c)
    arrC :: (a -> b) -> (a -> State b)

これは以下のように簡略化できる。

    bindC :: State a -> (a -> State b) -> State b
    retC :: a -> State a

MaybeとStateの比較
------------------

Maybeをつなぐときに使った関数は

    retM :: a -> Maybe a
    bindM :: Maybe a -> (a -> Maybe b) -> Maybe b

Stateをつなぐときに使った関数は

    retC :: a -> State a
    bindC :: State a -> (a -> State b) -> State b

これらの型はMaybeとStateを置き換えただけになっている。
共通する構造を抽出すると以下のようになる。

    ret :: a -> m a
    bind :: m a -> (a -> m b) -> m b

モナド
------

### モナドとは

この2つの関数を持つ型mを持ちモナド則を満たすものをモナドと呼ぶ。
これらの関数は以下の関数の簡略化したものと考えられる。

    arr :: (a -> b) -> (a -> m b)
    pipe :: (a -> m b) -> (b -> m c) -> (a -> m c)

モナド則

1. ret `pipe` fはfと同じである
2. f `pipe` retはfと同じである
3. (f `pipe` g) `pipe` hとf `pipe` (g `pipe` h)は同じである

これは簡単に言うと以下のことを意味する。

* retは値を包み込むだけでそれ以外のことをしない
* 変換関数を左結合にしても右結合にしても同じ

retを空文字列に、関数を文字列にそれぞれ対応させて考えるとわかりやすい。

    "" ++ "hello" == "hello"
    "hello" ++ "" == "hello"
    ("hello" ++ "my") ++ "friend" == "hello" ++ ("my" ++ "friend")

### まとめ

型mについて、以下の型の関数が存在し

    a -> m a
    m a -> (a -> m b) -> m b

それがモナド則を満たしさえすれば、型mはモナドである。
MaybeやStateはモナドである。

中身が何であれ関係なく、上記の条件を満たせば、すべてモナドである。

つまり、「モナド」とは内容であるよりも、形式である。
MaybeとStateのあいだにはほとんど共通点はない。
ただ、モナドという形式を満たすということだけが共通点である。

よりイメージしやすいモナド関数の型は以下のようになる。

    (a -> b) -> (a -> m b)
    (a -> m b) -> (b -> m b) -> (a -> m c)

言葉で言うと

* 普通の関数を「値をモナドにする関数」に変換できる
* 「値をモナドにする値数」同士をつなぐことができる

Monadクラス
-----------

値を比較できるもののためにEqクラスが用意されている。
同様にモナドに対してはMonadクラスが用意されている。
モナドクラスの定義は以下のようになる。

    class Monad m where
        (>>=) :: m a -> (a -> m b) -> m b
        return :: a -> m a

つまり(>>=)とreturnを定義してやれば、Monadのインスタンスになる。

Maybeモナド
-----------

### インスタンス宣言

Maybe型はデフォルトでモナドクラスのインスタンスになっている。
インスタンス宣言は以下のようになる。

    instance Monad Maybe where
        Nothing >>= _ = Nothing
        Just x >>= f = f x
        return = Just

### 文字コードを4分の1にする例

#### 変数を使わない定義

maybe.hsに以下を書きこむ。

    lowerToCodeDiv4' :: Char -> Maybe Int
    lowerToCodeDiv4' = lowerToCode c >>= evenDiv2 >>= evenDiv2

試してみる。

    *Main> :load maybe.hs
    *Main> lowerToCodeDiv4' 'n'
    Nothing
    *Main> lowerToCodeDiv4' 'p'
    Just 28

#### 変数を使う定義

同じことだが以下のように書くこともできる。

    lowerToCodeDiv4'' c =
        lowerToCode c >>= \n ->
        evenDiv2 n >>= \n' ->
        evenDiv2 n'

これは以下のように読める。

* lowerToCode cが返す値でnを束縛して
* evenDiv2 nが返す値でn'を束縛して
* evenDiv2 n'の値を返す

#### do記法による定義

do記法という構文糖がある。
それを使うと同じことが以下のように書ける。

    lowerToCodeDiv4''' c = do
        n <- lowerToCode c
        n' <- evenDiv2 n
        evenDiv2 n'

doという識別子で始める。
それぞれの行で以下の変換が行われる。

       [変数] <- [表現]
    -> [表現] >>= \[変数]

Stateモナド
-----------

まとめ
------
