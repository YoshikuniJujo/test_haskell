* 初級編
	+ 第1回 ミニマルな本質
	+ 第2回 特徴
	+ 第3回 最小限の意味論と構文糖
	+ 第4回 リスト
	+ 第5回 再帰関数
	+ 第6回 IOモナド
	+ 第7回 型クラス (newtypeの話題もここに入れよう)
	+ 第8回 モジュールシステム
	+ 第9回 落ち穂拾い
	+ 第10回 ghcの使いかた (runghc, ghciやその他のオプションについて)
* 中級編
	+ 第11回 ファイル入出力
	+ 第12回 ドキュメンテーション(haddock)
	+ 第13回 ランダム
	+ 第14回 テスト(型によるチェック、展開による証明、doctest, quickcheck, hunit)
	+ 第15回 モナド
	+ 第16回 いろいろなモナド
	+ 第17回 型族
	+ 第18回 モナド変換子
	+ 第19回 差分リスト
	+ 第20回 Showクラス
	+ 第21回 存在型とcast
	+ 第22回 ScopedTypeVariables拡張
	+ 第23回 型の階層
	+ 第24回 例外処理
	+ 第25回 入出力例外
	+ 第26回 プロファイリング
	+ 第27回 正格評価(foldl'あたりから入ると良さそうだ、bottomについては上級編で)
	+ 第28回 パターンマッチと正格性(遅延パターン、bang pattern等)
	+ 第29回 型構築子と正格性
	+ 第30回 zipper
	+ 第31回 重み平衡木
	+ 第32回 辞書
	+ 第33回 遅延リストとしての文字列(String)
	+ 第34回 配列としての文字列(ByteString, Text)
	+ 第35回 配列の遅延リストとしての文字列(ByteString.Lazy, Text.Lazy)
	+ 第36回 OverloadedStrings拡張
	+ 第37回 不変配列
	+ 第38回 MultiParamTypeClasses拡張
	+ 第39回 FlexibleInstances拡張
	+ 第40回 可変配列(IOArray, IOUArray, MArray)
	+ 第41回 unbox型
	+ 第42回 C言語とのインターフェース1
	+ 第43回 C言語とのインターフェース2 (値、配列、構造体、Storable)
	+ 第44回 C言語とのインターフェース3 (wrapper, StablePtr)
	+ 第45回 hsc2hs (第43回のmessageの例も使える, 構造体の例も, 小さな関数の同梱)
(ここまで作った、以下予定)
	+ 第46回 パッケージ化(cabalの使いかた(テストの自動化も含めて, hsc2hsの自動起動も))
		cabal-installによるhackageからのダウンロードやインストールも
* 上級編
	+ 第47回 functor, applicative, monad (MonadPlusの話題にも触れたい)
	+ 第48回 free monad
	+ 第49回 operational monad
	+ 第50回 forall(ランクN多相)
	+ 第51回 ST monad
	+ 第52回 ST monadを使った可変配列(STArray, STUArray)
	+ 第53回 並行実行
	+ 第54回 ref, mvar, chan, tvar
	+ 第55回 stm
	+ 第56回 正格評価、非正格評価、グラフ簡約
	+ 第57回 正規形、頭部正規形、弱頭部正規形
	+ 第58回 deepseq
	+ 第59回 並列実行
	+ 第60回 GADTs
	+ 第61回 template haskell
	+ 第62回 QuasiQuotes
	+ 第63回 parser (papillon の紹介)
	+ 第64回 unsafePerformIO, unsafeInterleaveIO
	+ 第65回 遅延IOと例外処理
	+ 第66回 iteratee
	+ 第67回 可変長引数 (Text.Printf等)
	+ 第68回 継続モナド
	+ 第69回 Arrow
	+ 第70回 Foldable
	+ 第71回 Traversable
	+ 第72回 Sequence (finger tree, queue)
	+ 第73回 hackageへのアップロードのしかた
* 番外編
	+ 第74回 readsPrecについて
	+ 第75回 モナド内包表記 monad comprehension
	+ 第76回 LANGUAGEプラグマ
	+ 第77回 その他のプラグマ
		(OPTIONS_GHC, WARNING, DEPRECATED, INLINE, RULES,
			SPECIALIZE, UNPACK, SOURCE)
		-> 場合によってはいくつかに分けよう
	+ 第78回 ghcのcore言語
	+ 第79回 ghcのSTG言語
* アルゴリズム編
	+ 第80回 wheel sieve
* パッケージ編
	+ 第81回 yesod

どこかで queue にも触れたいな
finger tree面白いな

Preludeについても触れておく必要があるな
あとは let ... in ..., where ...,
newtype もどこかで触れなければならないな。
Int と Integer について、も。
field label についても、か。
pattern guardはHaskell 2010に入ってるので初級編で触れておこうかな。
view patternもついでに
ffiも2010に入ってるな。
型注釈についても触れとこう。
それとas patternもだな。
irrefutable patternも。

bang patternについてはどこか別の場所で触れる必要がある。

ST monad や stm についても触れたい。
ST monad について触れるなら forall についても触れる必要があるだろう。
stm について触れる際に ref, mvar, chan, tvar あたりにも触れることにする。

第7回のキューの例をよりスムーズにするために、
第6回のIOのところでIOに関する再帰的プロセスと反復的プロセスについて
例を挙げておく必要があるだろう。

並列実行について触れるかどうか。
並行実行については触れておきたいな。<- stmより前がいいだろう。

Foldableについても触れたいな。
Traversableについても触れる必要があるかも。

RULESも面白い機能ではある。
INLINEについても見ておく必要がある。

ViewPatternsは効率に関する問題がある気がする。
初級編からは消そうかな。

GeneralizedNewtypeDerivingも「いけてる」気がする。

どこかに時間の扱いも入れときたいな。

どこかでMonadPlusについても触れたいな。

以下の型レベルの機能については理解したうえで入れるかどうか検討すること
	DataKinds, TypeOperators, ConstraintKinds

どこかで「代数的データ型(algebraic data type)」という用語の説明が必要だ。

kindについてもどこかで触れたい。
GADTsあたりで触れられるだろうか。

guardについてどこかで触れる必要がある。

「入出力例外」の回は不要かもしれないな。

どこかでfoldl'の紹介はしたい

演算子の優先順位について触れているかどうかチェックすること
	-> 落ち穂拾いあたりにいれとこうかな?

第22回でasTypeOfについて触れること!
