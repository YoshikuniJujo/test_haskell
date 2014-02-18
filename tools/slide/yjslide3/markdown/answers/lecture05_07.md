第5回 演習7 (解答)
==================

問い
----

リストをペアに区切る関数をpairsとする。

pairsを定義せよ。

考えかた
--------

以下のようなリストがあったとする。

    lst = [x, y, z, w, v, u ... ]

ひとつずらしたリストは

    tail lst
    [y, z, w, v, u ...]

それともとのリストとのzipは

    zip lst $ tail lst
    [(x, y), (y, z), (z, w), (w, v), (v, u) ...]

これをひとつ置きにとると

    hop $ zip $ tail lst
    [(x, y), (z, w), (v, u) ...]

解答
----

    pairs lst = hop $ zip lst $ tail lst
