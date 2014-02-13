第2回 型
========

はじめに
--------

値には型がある。
型は「その値にどの関数を適用できるか」や
「どの関数がその型を返すか」を示す。
つまり、「その値で何ができるか」を表すもの。

関数にも型がある。
関数の型は引数と返り値の型で定義される。
複数の型に適用できる関数があり、多相関数と呼ばれる。
多相関数の型は型変数を使って表現される。

型の導出はHaskellを学ぶ際のよい練習となる。

型とは:「何ができるか」
----------------------

値には型があり、型によって「何ができるか」が決まる。

Haskellには文字を表すChar型が用意されている。
文字はtoLowerで小文字にすることができる。

    Prelude> :m Data.Char
    Prelude Data.Char> toLower 'J'
    'j'

toLowerを含むData.Charモジュールを読み込んでから、
'J'にtoLowerを適用している。

型の不適合
----------

真偽を表現するBool型がある。
Bool型はFalse, Trueの2つの値のみを持つ。
たとえば、Falseを小文字にすることはできない。

    Prelude Data.Char> toLower False

    <interactive>:3:9:
        Couldn't match expected type `Char' with actual type `Bool'
        In the first argument of `toLower', namely `False'
        In the expression: toLower False
        In an equation for `it': it = toLower False

「FalseはBoolであってCharじゃないよ」と言われる。

関数の型
--------

### 何によって決まるか

関数の型は引数の型と返り値の型によって決まる。
たとえばtoLowerは文字を取って文字を返す関数なので型はChar -> Charとなる。

### 例

「関数」の回で見た関数bmiの型はどうなるだろうか。

入力と出力を倍精度浮動小数点数とする。
倍精度浮動小数点数を表す型はDoubleなので、
bmiは「Doubleを2こ取ってDoubleを返す関数」となる。

2引数関数は本当は「関数を返す関数」なので

    Double -> (Double -> Double)

 ->は右結合なので()は省略できる。

    Double -> Double -> Double

### 一般的な形

以下のような形となる。

    [引数1の型] -> [引数2の型] -> ... -> [返り値の型]

型の重要性
----------

関数を考えるときにはまず型を考えると良い。
つまり、関数の中身を作る前に以下を考えるということ。

* 返り値としてほしいのはどういう種類の値か
* 関数の入力には何が必要か

これらを考えることは頭を整理することにつながる。

bmiの例では、ほしいのは「BMIを表す浮動小数点数」。
必要なのは「身長と体重を表す2つの浮動小数点数」。

別の例として「BMI値によって肥満かどうかを判定する関数」を考えよう。
この場合、ほしいのは「肥満かどうかを表すBool値」。
入力は同上の「2つの浮動小数点数」。

型の宣言
--------

### 型宣言の重要性

bmi関数を定義したときには型を宣言しなかった。
型を宣言しない場合、型はコンパイラによって推論される。
しかし、以下の理由から型は宣言したほうが良い。

* 自分が何を作っているのかを明確にできる
* バグの多くは型チェックによって検出可能
* 型宣言は優れたドキュメントとなる
* こみいった型だと型推論が利かない場合がある

### 型宣言の記法

    [関数名] :: [型]

型駆動開発
----------

### 型駆動開発とは

「テスト駆動開発」を見て思いついた言葉。
まずは型を決めてそれから中身をうめていくやりかた。

### isObeseを作る

#### 問題定義

身長と体重から肥満かどうかを判定する関数isObeseを作る。

#### 型宣言

ディレクトリlectures/lecture02/を作り、
lectures/lecture02/bmi.hsを作ろう。

    bmi :: Double -> Double -> Double
    bmi = undefined

    isObese :: Double -> Double -> Bool
    isObese = undefined

undefinedは評価されるとエラーとなる値。
スタブとして使用した。

#### 対話環境での確認

    % ghci bmi
    *Main> bmi 218 164
    *** Exception: Prelude.undefined
    *Main> isObese 218 164
    *** Exception: Prelude.undefined

当然エラーとなる。

    *Main> :type bmi
    bmi :: Double -> Double -> Double
    *Main> :type isObese
    isObese :: Double -> Double -> Bool

対話環境では:typeで関数や値の型を表示できる。

#### トップダウン開発

開発のしかたにはボトムアップとトップダウンとがある。
今回の例ではbmi関数を先に定義すればボトムアップとなり、
isObese関数を先に定義すればトップダウンとなる。

今回は、トップダウンで開発する。

##### 関数isObeseの定義

BMIが30以上で肥満とされるので

    isObese h w = bmi h w >= 30

これをbmi.hsに書き込もう。

ここでの思考は以下のようになる。

1. 身長と体重からBMIを求める関数bmiがあると仮定する
2. isObese h wはbmi h w >= 30である

##### 関数bmiの定義

bmi関数が定義されているという仮定のもとでisObese関数が定義できた。
よってbmi関数を定義すれば仕事は終わる。

    bmi h w = w / (h / 100) ^ 2

##### 対話環境での確認

動かしてみよう。

    % ghci bmi.hs
    *Main> isObese 218 164
    True
    *Main> isObese 218 140
    False

チェ・ホンマンはかつて肥満であったとの判定。
ただし、BMIは高身長者には厳しく、また脂肪と筋肉を区別しないので
そこのところは注意が必要である。

演算子の型宣言
--------------

もちろん演算子についても型宣言ができる。
考えかたとしては()でくくって「関数」にするということ。

(&&)演算子の例を見る。

    *Main> False && True
    False
    *Main> True && True
    True

Bool値を2つとりBool値を返す関数(演算子)なので

    (&&) :: Bool -> Bool -> Bool

多相関数
--------

複数の型に適用可能な関数がある。

### idの例

#### idとは

非常に単純な関数としてidがある。

    *Main> id 'c'
    'c'
    *Main> id False
    False

定義は以下のようになる。

    id x = x

つまり引数をそのまま返す関数である。

引数が何であったとしても自分自身を返すことは可能なので、
すべての型の値を引数とすることができる。

#### idの型宣言

idの関宣言を書きたい。

id 'c'のときのidの型は

    Char -> Char

id Falseのときのidの型は

    Bool -> Bool

つまり[型X] -> [型X]という形となる。

このようにどんな型でも良いという場合には型変数が使える。
型変数は小文字で始まる識別子である。
よって以下のようにできる。

    id :: a -> a

これは以下のように読める。
「idは型aの値をとり、同じ型aの値を返す関数」

### 関数(.)の例

(.)の型について見ていく。

#### chr, isLowerを使った例

引数の例として以下の関数を使う。

    chr :: Int -> Char
    isLower :: Char -> Bool

やってみよう。

    *Main> :m Data.Char
    Prelude Data.Char> chr 102
    'f'
    Prelude Data.Char> chr 85
    'U'
    Prelude Data.Char> (isLower . chr) 102
    True
    Prelude Data.Char> (isLower . chr) 85
    False

それぞれの型を見ると

    isLower :: Char -> Bool
    chr :: Int -> Char
    isLower . chr :: Int -> Bool

(.)の引数と返り値として考えると

    引数1: Char -> Bool
    引数2: Int -> Char
    返り値: Int -> Bool

よって(isLower . char)のときの(.)の型は

    (Char -> Bool) -> (Int -> Char) -> (Int -> Bool)

#### 一般化する

まずは(.)の定義を見てみる。

(f . g)は「引数にgを適用した結果にfを適用する関数」なので

    (.) f g = \\x -> f (g x)

xの型をaとするとgの型は「aの型の値を取って何らかの値を返す関数」なので

    g :: a -> b

するとg xの型はbとなり、それがfの引数となるのでfの型は
「bの型の値を取って何らかの値を返す関数」となるので

    f :: b -> c

するとf (g x)の型はcとなる。xの型がaなので

    \x -> f (g x) :: a -> c

(.)について見ると、f, gが引数で(\x -> f (g x))が返り値となるので

    (.) :: (b -> c) -> (a -> b) -> (a -> c)

まとめ
------

* 型とは値の種類
* 型が同じなら値によらずできることは同じ
* 関数を作るときにはまず型を考える
    - 型が決まれば仕事は半分終わったようなもの
    - 型チェックによってバグの多くは検出できる
    - 型はドキュメントとしても優れる
* 型の宣言は[関数名] :: [型]という形
* 多相関数の型宣言には型変数を使う
* 型の導出は勉強になる
