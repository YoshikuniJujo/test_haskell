第5回 演習11 (解答のヒント)
===========================

問い
----

点を打つ個数とランダムの種を入力として、
円周率の、モンテカルロ法による、近似値を出力つする関数をgetPiとし、
getPiを以下のように定義する。

    getPi n sg = 4 * fromIntegral (length inps) / fromIntegral n
        where inps = [ (a) ]

[ (a) ]に入る式を求めよ。

考えかた
--------

円のなかかどうかを表す関数は

    inCircle :: Double -> Double -> Bool

点の表現は(Double, Double)なので、非カリー化する。

    uncurry :: (a -> b -> c) -> (a, b) -> c

また、n個のランダムな点は以下で表すことができる。

    take n $ points sg

問題の再定義
------------

ランダムな点の個数と種sgが与えられると、
ランダムな点のリストはtake n $ points sgで表せる。

* 演習11'. その点のうち単位円内にある多のリストを求めよ(1分)
