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

### arrM

### bindMとretM

### まとめ

Stateをつなげる
---------------

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
