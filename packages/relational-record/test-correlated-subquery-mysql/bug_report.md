HRRの相関クエリに関するバグ報告
===============================

バグの具体例による説明
----------------------

```haskell
updatePersonByMoney :: Update ()
updatePersonByMoney = update $ \proj -> do
	canBuy' <-# value (1 :: Int32)
	(wheres . exists =<<) . queryList . relation $ do
		m <- query monies
		wheres $ m ! Mny.pid' .=. proj ! Psn.id'
		wheres $ m ! Mny.money' .>=. value 100
		return (value (1 :: Int32))
	return unitPlaceHolder
```

上記のようなupdatePersonByMoneyをshowすると、
だいたいつぎのようなSQLになります。

```sql
UPDATE test.persons SET can_buy = 1
	WHERE EXISTS (
		SELECT ALL 1 AS f0 FROM test.monies T0
			WHERE T0.pid = id AND T0.money >= 100 )
```

ふたつのテーブルがつぎのように定義されているとします。

* CREATE TABLE persons (id int not null, can\_buy int not null)
* CREATE TABLE monies (id int not null, pid int not null, money int not null)

このとき、WHERE T0.pid = idにおけるidはテーブルmoniesのほうの列idとして、
解釈されてしまうようです。

### データの例

#### 初期値

persons

* id, can\_buy
* 1, 0
* 2, 0
* 3, 0

monies

* id, pid, money
* 1, 1, 120
* 2, 2, 90
* 3, 3, 110

#### クエリーの実行後

##### 求める結果

persons

* id, can\_buy
* 1, 1
* 2, 0
* 3, 1

##### 実際の結果

persons

* id, can\_buy
* 1, 1
* 2, 1
* 3, 1

実際のコード
------------

app/Main.hsにバグの例と、ほぼおなじで、列idを持たないmonies2による例との
実行例を示しました。

```
% stack build
% stack exec test-correlated-subquery-mysql-exe
```
