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

### 必要な関数とその型

### mplus

### mrecall

### arrC

### Calc型

### pipeC

### 計算例

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
