memo
====

モジュール構造
--------------

```
Main
  +- Trials.Boxes
  |    +- Trials.Boxes.Events
  |    |    +- Trials.Boxes.Mouse
  |    +- Trials.Boxes.View
  +- Trials.Boxes.Run
       +- Trials.Boxes.Handle
       |    +- Trials.Boxes.Events
       +- Trials.Boxes.View
```

バグフィックス
--------------

### スリープに関する話

#### 要求

* 複数のスリープが並行して実行されることがある
* ひとつのスリープイベントがすべてのスリープにわたされる
* 一番小さいスリープ要求からスリープイベントを生成する
* ビジーループにならないようにする
* DeltaTimeとの関係
* Mouse Eventとの関係

### 解決策

* 実際の時間になるまでTryWaitだけをあつかう?
	+ DeltaTimeイベントは発生させ続けるけど
	+ beforeとmergeだとうまく対応できない?

```
ハンドル(A)

マウスイベントを取得
    |
    |
    V
現在時刻を取得
    |
    |
    V                        (no)
一番小さいスリープより小さい ----> 最小スリープから得たSleep, DeltaTimeイベントを発生させる ----> マウスイベントとタイムイベントを返す
  または、スリープ要求がない                                                                         |
    | (yes)                                                                                          |
    |                                                                                                V
    |                                                                                             現在時刻を引数にしてハンドル(B)ヘ
    |
    |
    V
現在時刻から得たSleepイベント、DeltaTimeイベントを発生させる
    |
    |
    V
現在時刻を記録
    |
    |
    V
マウスイベントとタイムイベントを返す


ハンドル(B) (引数: 現在時刻)
                                       (no)
現在時刻が一番小さいスリープより小さい ----> 最小スリープから得たSleep, DeltaTimeイベントを発生させる ----> マウスイベントとタイムイベントを返す
  または、スリープ要求がない                                                                                  |
    | (yes)                                                                                                   |
    |                                                                                                         V
    V                                                                                                       おなじ引数でハンドル(B)ヘ
現在刻から得たSleepイベント、DeltaTimeイベントを発生される
    |
    |
    V
タイムイベントを返す
    |
    |
    V
ハンドル(A)ヘ
```

* ハンドルを切りかえるという話
	+ ひとつのやりかたとしては、追加の引数と追加の返り値で状態を渡していくという作り
	+ interpret'みたいな感じで

リファクタリング
----------------

* [ ] module hierarchy
	+ [ ] Trials.Boxes.Mouse
	+ [ ] Trials.Boxes.Mouse
		- -> MonadicFrp.Events.Mouse
		- -> MonadicFrp.XHandle.Mouse

### API

* [ ] Trials.Boxes.Events
	+ [x] 1st
	+ [ ] 2nd
		- [ ] Occurred ?
		- [ ] others
* [ ] Trials.Boxes.Mouse
* [ ] Trials.Boxes
* [ ] Trials.Boxes.Handle
* [ ] Trials.Boxes.TryReact
* [ ] Trials.Boxes.TrySig

### Body

* [ ] Trials.Boxes.Events
* [ ] Trials.Boxes
* [ ] Trials.Boxes.Handle
* [ ] Trials.Boxes.TryReact
* [ ] Trials.Boxes.TrySig
* [ ] Main
