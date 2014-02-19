第5回 演習11' (解答)
====================

問い
----

ランダムな点の個数nと種sgが与えられてると、
ランダムな点のリストはtake n $ points sgとなる。

それらの点のうち単位円内にある点のリストを求めよ。

考えかた
--------

ランダムな点のリストは

    take n $ points sg

ある点が単位円内にあるかどうかをチェックする関数は

    uncurry inCircle :: (Double, Double) -> Bool

この関数でランダムな点のリストをfilterすれば良い

解答
----

    filter (uncurry inCircle) $ take n $ points sg

getPi関数
---------

でき上がった関数は以下のようになる。

    getPi :: Int -> StdGen -> Double
    getPi n sg = 4 * fromIntegral (length inps) / fromIntegral n
        where
        inps = filter (uncurry inCircle) $ take n $ points sg
