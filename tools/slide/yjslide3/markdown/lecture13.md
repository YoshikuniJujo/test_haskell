第13回 型クラス
===============

はじめに
--------

複数の型に共通の性質が存在する。

* 大小比較ができる
* 文字列として表示できる
* 等々

ある型がその「性質」を持つということは、
その「性質」を表現する関数がその型に対して定義されている
ということと等しい。

つまり、(>)や(<)といった関数がIntに対して定義されているので、
Int型は「大小比較可」という性質を持つことになる。

型クラスとは
------------

Haskellではそのような「性質」を型クラスで表現する。
型TがクラスCで表される性質を持つということを
「型TはクラスCのインスタンスである」と表現する。

型クラスには以下のようなものがある。

* Ordは「大小比較可」という性質を表すクラスである
* Showは「表示可」という性質を表すクラスである

型クラスとインスタンスの例には以下のようなものがある。

* Int型はOrdクラスのインスタンスである
* Char型はShowクラスのインスタンスである
* 等々

対話環境における確認
--------------------

lectures/lecture13を作りそこに移動する。

### 型がどの型クラスに属するか

    % ghci
    Prelude> :info Char
    data Char = GHC.Types.C# GHC.Prim.Char#        -- Defined in `GHC.Types'
    instance Bounded Char -- Defined in `GHC.Enum'
    instance Enum Char -- Defined in `GHC.Enum'
    instance Eq Char -- Defined in `GHC.Classes'
    instance Ord Char -- Defined in `GHC.Classes'
    instance Read Char -- Defined in `GHC.Read'
    instance Show Char -- Defined in `GHC.Show'

ある型がどのクラスのインスタンスであるかを調べるには、
ghciで:info [型名]とする。
ここでは、CharがOrdやShowのインスタンスであることがわかる。

### 型クラスの持つクラス関数

ある型クラスにどんなクラス関数があるかを知るには

    Prelude> :info Ord
    class Eq a => Ord a where
        compare :: a -> a -> Ordering
        (<) :: a -> a -> Bool
        (>=) :: a -> a -> Bool
        (>) :: a -> a -> Bool
        (>=) :: a -> a -> Bool
        max :: a -> a -> a
        min :: a -> a -> a
              -- Defined in `GHC.Classes'
    instance Ord a => Ord (Maybe a) -- Defined in `Data.Maybe'
    instance (Ord a, Ord b) => Ord (Either a b)
      -- Defined in `Data.Either'
    instance Ord Integer -- Defined in `integer-gmp:GHC.Integer.Type'
    ...
    instance (Ord a, Ord b) => Ord (a, b) -- Defined in `GHC.Classes'
    instance Ord () -- Defined in `GHC.Classes'

Ordクラスのクラス関数にはcompare, (<)などがあることがわかる。
また、'Eq a =>'とあるのは
「Ordクラスのインスタンスであるためには
Eqクラスのインスタンスである必要がある」
ということ。

また「どの型がこの型クラスのインスタンスであるか」もわかる。

Size型をOrdクラスのインスタンスにする
-------------------------------------

### はじめに

型クラスは、もちろん、自分で作れるが、
まずは自作の型を既存の型クラスのインスタンスにしてみる。

### Size型の定義

スターバックスのカップのサイズを表す型を作る。

    data Size = Short | Tall | Grande | Venti

これをclass.hsに保存する。

これは大小比較可能なのでOrdクラスのインスタンスにする。
Ordクラスのインスタンスにするには、
まずはEqクラスのインスタンスにする必要がある。

### Eqクラス

Eqクラスは同値かどうかを判定可能という性質を表すクラスである。

Eqクラスのクラス関数には(==), (/=)がある。
このクラスのインスタンスにするには(==)だけを定義すれば良い。
(==)を定義しておけば(/=)にはデフォルトの定義が使われる。

実行効率あるいはその他の理由で(/=)を別に定義することもある。

実際にSizeをEqクラスのインスタンスにしてみよう。

    instance Eq Size where
        Short == Short = True
        Tall == Tall = True
        Grande == Grande = True
        Venti == Venti = True
        _ == _ = False

これをclass.hsに書きこみ、試してみる。

    Prelude> :load class.hs
    *Main> Short == Short
    True
    *Main> Tall == Venti
    False
    *Main> Grande /= Grande
    False
    *Main> Venti /= Short
    True

### Ordクラス

Ordクラスには7つのクラス関数がある。

    compare, (<), (>=), (>), (<=), max, min

すべて定義しても良いが、compareまたは(<=)のどちらかを定義すれば、
他の関数はデフォルトの値が使われる。

SizeをOrdのインスタンスにする。

    instance Ord Size where
        Short <= _ = True
        _ <= Short = False
        Tall <= _ = True
        _ <= Tall = False
        Grande <= _ = True
        _ <= Grande = False
        Venti <= _ = True

これをclass.hsに書き込み、試してみる。

    *Main> :reload
    *Main> Short < Short
    False
    *Main> Grande <= Grande
    True
    *Main> Tall >= Venti
    False
    *Main> Venti > Short
    True

ここまでのまとめ
----------------

複数の型に共通した性質がある。
「性質」はその型を扱う関数によって表現される。
そのような性質、つまり関数をまとめたものが型クラスである。

型クラスのインスタンスにするには関数の中身を定義すれば良い。
構文は以下のようになる。

    instance [型クラス] [型名] where
        [関数定義1]
        [関数定義2]
        ...

deriving
--------

型SizeをOrdクラスのインスタンスにするのはもっと簡単にできる。

    data Size = Short | Tall | Grande | Venti deriving (Eq, Ord)

使用頻度の高い以下のクラスについてはderivingで簡単にインスタンスを導出できる。

    Eq, Ord, Enum, Ix, Bounded, Show, Read

derivingを使う場合は値構築子を「小さいものから大きいものへ」の順に
並べておくと良い。

型クラスを作る: オートマトンの例
--------------------------------

### はじめに

オートマトンという数学的なモデルがある。
入力によって次々に状態を変化させていくものであり、
初期状態と受理状態とを持つ。

初期状態に対して次々と入力を与えていき、
入力が終わったときに状態が受理状態であれば、
その入力列を「受理する」ことになる。

単純な機械をモデル化したものと考えられる。

これを型クラスという仕組みを活用して作っていこう。

### オートマトンの表記

オートマトンは以下のように表記される。

* 状態を丸で表し、状態間の遷移を矢印で示す
    + 矢印のそばにその遷移を引き起こす入力を書く
* 初期状態には矢印を追加する
* 受理状態は二重丸とする

オートマトンの例を以下に示す。これを例1としよう。

### オートマトンの例1

#### 定義

![automatonImage1](automatonImage1.png "large")

入力列が0, 1, 1, 0, 0の場合

    q1 -> q1 -> q2 -> q2 -> q3 -> q2

q2は受理状態なのでこの入力列は受理される。

このオートマトンが受理するのは以下の入力列となる。

* すくなくともひとつの1を含み
* 最後の1のあとには偶数個(0個も可)の0が来る

いろいろな例で確認してみる。

#### 入力列の例

##### 入力が1で終わる場合

入力が1で終わる場合は必ず受理される。

    1: q1 -(1)-> q2
    01: q1 -(0)-> q1 -(1) -> q2
    11: q1 -(1)-> q2 -(1) -> q2
    0101: q1 -(0)-> q1 -(1)-> q2 -(0)-> q3 -(1)-> q2

どの状態からでも1が来れば受理状態であるq2に遷移するので、
入力の最後が1ならば必ず受理される。

##### 入力に1が含まれない場合

入力に1が含まれない場合は決して受理されない。

    (入力無し): q1
    0: q1 -(0)-> q1
    000: q1 -(0)-> q1 -(0)-> q1 -(0)-> q1

q1から出るにはすくなくともひとつの1が必要なので、
0のみの入力ではq1にとどまり続ける。

q1は受理状態ではないので、これらは受理されない。

##### 最後の1の後に奇数個の0が続く場合

最後の1の後に奇数個の0が続く場合には受理されない。

    10: q1 -(1)-> q2 -(0)-> q3
    1000: q1 -(1)-> q2 -(0) -> q3 -(0)-> q2 -(0)-> q3

##### 最後の1の後に偶数個の0が続く場合

最後の1の後に偶数個の0が続く場合には受理される。

    100: q1 -(1)-> q2 -(0)-> q3 -(0)-> q2

最後の1の後は0が来るたびにq2とq3を交互に遷移するため。

#### 説明

* 1が入力されなければq1にとどまる
* 1が来た時点で必ず状態q2にいる
* よって、最後の1以前の入力は無視できる
* 最後の1以降には0しか来ない
* 0が来るたびにq2とq3のあいだを行き来するので
    + 偶数個の0ならば受理され
    + 奇数個の0ならば受理されない

### 状態であるという性質を表すクラス

#### AMStateクラスに必要なもの

ある型がオートマトンの状態であるということを表すAMStateクラスを作る。

入力値は0, 1値とし、それを以下で表現する。

    data OI = O | I deriving Show

アルファベットのOとIを0と1に見立てている。
これをautomaton.hsに書きこむ。

オートマトンの状態であるために必要なのは何かを考える。
以下の3つが必要となる。

1. ある状態が入力によってどの状態に移るか
2. 初期状態
3. 状態が受理状態であるかどうかのチェック

#### クラス関数の型

オートマトンの状態を表す型を型変数qで表すことにする。

##### 関数step

関数stepは「1. 状態の遷移」を表す。

これは現在の状態と入力値を引数として、遷移先の状態を返す関数なので

    step :: q -> OI -> q

##### 関数start

関数startは「2. 初期状態」を表す。
初期状態は状態なので

    start :: q

##### 関数accept

関数acceptは「3. 状態が受理状態であるかどうかをチェック」する。
つまり、状態をとりBool値を返せば良い。

    accept :: q -> Bool

#### クラス宣言の構文

クラス宣言は以下のような構文となる。

    class [クラス名] [型変数] where
        [クラス関数名1] :: [型1]
        [クラス関数名2] :: [型2]
        ...

#### AMStateクラスの定義

よってオートマトンの状態であることを表すクラスは

    class AMState q where
        step :: q -> OI -> q
        start :: q
        accept :: q -> Bool

これは、どんな型であっても、step, start, acceptを定義しさえすれば
「オートマトンの状態」となるということ。

### オートマトンを実行する関数

#### 関数run

AMStateのインスタンスに対する関数を作ることができる。

入力列に対する状態遷移の結果を返す関数を定義する。

    run :: AMState q => q -> [OI] -> q
    run q [] = q
    run q (oi : ois) = run (step q oi) ois

入力がなければ状態はそのままであり、
入力があれば、その入力に対して状態を遷移させたうえで入力を続ける、ということ。

これをautomaton.hsに書き込む。

#### 関数isAccept

ある入力列が受理されるかどうかを判定する関数は

    isAccept :: AMState q => [OI] -> Bool
    isAccept = accept . run start

で良さそうだが、これは動かない。

isAcceptの引数は[OI]で返り値はBoolである。
どこにもqが出てこないのでstartの型を決めることができない。

このようなときはダミーの引数を使う。

    isAccept :: AMState q => q -> [OI] -> Bool
    isAccept q = accept . run (start `asTypeOf` q)

asTypeOfは第1引数の型を第2引数の型に合わせるための関数である。

これをautomaton.hsに書きこむ。

### 例1の実装

#### 例1のオートマトン

![automatonImage1](automatonImage1.png "large")

#### 型の定義

型名をAM1として、型の値をQ1, Q2, Q3とする。

    data AM1 = Q1 | Q2 | Q3 deriving Show

#### 関数stepAM1の定義

関数stepAM1を定義しよう。これは遷移のルールを決める。

    stepAM1 :: AM1 -> OI -> AM1
    stepAM1 Q1 O = Q1
    stepAM1 Q1 I = Q2
    stepAM1 Q2 O = Q3
    stepAM1 Q2 I = Q2
    stepAM1 Q3 _ = Q2

これをautomaton.hsに書きこむ。

#### AMStateのインスタンスにする

* stepはstepAM1
* 初期状態はQ1
* 受理状態は「Q2であること」

インスタンス宣言は以下のようになる。

    instance AMState AM1 where
        step = stepAM1
        start = Q1
        accept Q2 = True
        accept _ = False

automaton.hsに書きこむ。

#### isAcceptの使いかた

ここでisAcceptの使いかたを見てみよう。型は以下のようになる。

    isAccept :: AMState q => q -> [OI] -> Bool

isAcceptの第1引数はダミーの引数で、評価されることはなく、
型だけわかれば良い。

Haskellには評価されるとエラーを発生させるだけの値がはじめから用意されている。

    undefined :: a

undefinedはあらゆる型になれるのでisAcceptの第1引数は(undefined :: AM1)とする。

#### 試してみる

今考えているオートマトンが受理する入力列は、以下のようなものである。

    すくなくともひとつの1を含み、最後の1のあとには偶数個の0が並ぶ

これを念頭に置いて実際に試してみよう。

    *Main> :load automaton.hs
    *Main> isAccept (undefined :: AM1) []
    False
    *Main> isAccept (undefined :: AM1) [O]
    False
    *Main> isAccept (undefined :: AM1) [I]
    True
    *Main> isAccept (undefined :: AM1) [I, I, O, I, O, O]
    True
    *Main> isAccept (undefined :: AM1) [I, I, O, I, O, O, O]
    False

### 例2の実装

#### 例2のオートマトン

もうひとつの例を見てみよう。
以下のようなオートマトンを考える。

* 1の個数が奇数個である入力列を受理する

これは以下のようなオートマトンとなる。

![automatonImage2](automatonImage2.png "large")

1が来るたびに状態qeと状態qoのあいだを遷移し、
0の場合には同じ状態にとどまる。

初期状態はqeであり、受理状態はqoである。

#### 型の定義

型AM2を定義する。

    data AM2 = Qe | Qo deriving Show

automaton.hsに書きこむ。

#### 関数stepAM2の定義

関数stepAM2を定義する。

    stepAM2 :: AM2 -> OI -> AM2
    stepAM2 q O = q
    stepAM2 Qe I = Qo
    stepAM2 Qo I = Oe

automaton.hsに書きこむ。

#### AMStateのインスタンスにする

    instance AMState AM2 where
        step = stepAM2
        start = Qe
        accept Qo = True
        accept _ = False

automaton.hsに書きこむ。

#### 試してみる

    *Main> :reload
    *Main> isAccept (undefined :: AM2) []
    False
    *Main> isAccept (undefined :: AM2) [O]
    False
    *Main> isAccept (undefined :: AM2) [I]
    True
    *Main> isAccept (undefined :: AM2) [I, I, O]
    False
    *Main> isAccept (undefined :: AM2) [I, I, O, I]
    True

### まとめ

AMStateは「オートマトンの状態である」という性質をクラスにしたもの。

この例で見たように型クラスは、「性質」であると考えられるだけでなく、
「仕様と実装を分離するシステム」とも考えられる。

AMStateクラスはstep, start, acceptという関数を持つという「仕様」である。

クラスによって定められた「仕様」はインスタンス宣言のなかで「実装」を与えられる。

そして、型宣言内における「AMState q =>」は、型qがAMStateクラスの仕様を満たすことを保証してくれる。

まとめ
------

型クラスとは型の持つ「性質」を表現したものであり、
「性質」はその型を扱う関数で表現される。
「性質」は「仕様」と呼ぶこともできる。

クラス宣言ではクラス関数の型を定義する。
インスタンス宣言ではクラス関数の実装を作成する。

「[型クラス名] => a」とすることで、
aを与えられた型クラスのインスタンスに制限できる。
つまり、その型クラスのクラス関数の存在が保証される。
