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
