Freer Effects
=============

Extensible Effects
------------------

大きくわけると3つの技法から成る

* Freeモナド
* 存在型によるオープンな型
* 型演算によって操作を型安全にする

Freer Effects
-------------

Extensible Effectsを改良したもの。

* Freerモナド (多分Operationalモナド)
	- fmapによるオーバーヘッドをなくす
	- 多分、むきだしの継続を使わないことで、コードが書きやすくなる
* Freerモナドの改良
	- 関数を、そのまま合成するのではなく、
		一度FTCQueueに格納する

memo
----

### 関数decomp

Unionをcastするより、

	decomp :: Union (Hoge :> es) a -> Either (Hoge a) (Union es a)

のようにしたほうが、スマートかも。

### 関数send

関数sendを使って、Member eff effsを保証する。
