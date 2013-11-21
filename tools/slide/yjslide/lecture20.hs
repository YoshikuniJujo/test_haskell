import Lecture

subtitle :: String
subtitle = "第20回 存在型"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	showBox, showBox2, showBoxClass,
	typeable, cast, cast2, cast3, cast4,
	fromEx,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 存在型(existential types)について学ぼう", \t -> do
	text t "* Haskell 2010にはないGHCの拡張機能", \t -> do
	text t "* {-# LANGUAGE ExistentialQuantification #-}が必要", \t -> do
	text t "* 例えば以下のような定義が可能になる", \t -> do
	itext t 1 "data Any = forall t . Any t", \t -> do
	text t "* つまりdata宣言の左辺にない型変数が右辺で使える", \t -> do
	text t "* 型構築子の型を見てみる", \t -> do
	itext t 1 "Any :: t -> Anything", \t -> do
	text t "* つまりあらゆる型からAnything型が作れる"
 ]

showBox :: Page
showBox = [\t -> do
	writeTopTitle t "ShowBox"
	text t "", \t -> do
	text t "* Any型にはほとんど実用性がない", \t -> do
	text t "* あらゆる型をまとめることができるが", \t -> do
	itext t 1 "- それらの型に共通して使える関数があまりない", \t -> do
	text t "* ある程度実用的な例を考えてみる", \t -> do
	itext t 1 "data ShowBox = forall s . Show s => SB s", \t -> do
	text t "* 共通に使える関数としてshowを使うことができる", \t -> do
	text t "* リストを作ってみる", \t -> do
	itext t 1 "heteroList :: [ShowBox]"
	itext t 1 "heteroList = [SB (), SB 5, SB True]"
 ]

showBox2 :: Page
showBox2 = [\t -> do
	writeTopTitle t "ShowBox"
	text t "", \t -> do
	text t "* ShowBoxを扱う関数を作る", \t -> do
	itext t 1 "showSB :: ShowBox -> String"
	itext t 1 "showSB (SB s) = show s", \t -> do
	text t "* この関数は実のところ重要な性質を表現している", \t -> do
	itext t 1 "- SBはShowクラスに属する型sからShowBoxを作る", \t -> do
	arrowIText t 2 "(SB s)というパターンマッチはその逆である", \t -> do
	arrowIText t 2 "ShowBoxからShowクラスに属する任意の型を", \t -> do
	itext t 1 "- showはShowクラスに属する型sからStringを作る", \t -> do
	text t "* (SB s)でパターンマッチした値の型は発散している", \t -> do
	itext t 1 "- showによって特定の型に収束させる必要がある"
 ]

showBoxClass :: Page
showBoxClass = [\t -> do
	writeTopTitle t "ShowBox"
	text t "", \t -> do
	text t "* SBは複数の型をひとつの型に収束させる", \t -> do
	text t "* (SB s)はひとつの型を複数の型に発散させる", \t -> do
	text t "* showはそれら複数の型を再度単一の型に収束させる"
	text t "", \t -> do
	text t "* SBは型sからShowBoxを作る関数", \t -> do
	text t "* ShowBoxから型sを取り出すことは不可能", \t -> do
	itext t 1 "fromSB :: Show s => ShowBox -> s"
	itext t 1 "fromSB (SB x) = x", \t -> do
	itext t 1 "- sは任意の型でなければならない", \t -> do
	itext t 1 "- xの型はもとの(SB x)によって決まってしまう", \t -> do
	itext t 1 "- よって上の関数には型の不整合がある"
 ]

typeable :: Page
typeable = [\t -> do
	writeTopTitle t "Typeable"
	text t "", \t -> do
	text t "* Typeableクラス", \t -> do
	itext t 1 "class Typeable a where"
	itext t 2 "typeOf :: a -> TypeRep", \t -> do
	text t "* typeOfを使えば値の型を得ることができる", \t -> do
	text t "* 通常であればこの関数には実用性はない", \t -> do
	itext t 1 "- 通常の値であれば型は実行前に決定する", \t -> do
	text t "* 存在型に使うことで威力を発揮する", \t -> do
	itext t 1 "- (SB x)で得られるxの型は不定", \t -> do
	itext t 1 "- typeOf関数を使うことでTypeRep型に収束する"
 ]

cast :: Page
cast = [\t -> do
	writeTopTitle t "unsafeCoerce"
	text t "", \t -> do
	text t "* unsafeCoerceという関数がある", \t -> do
	itext t 1 "- unsafeCoerce :: a -> b", \t -> do
	text t "* unsafe...という名前の関数は「危険」", \t -> do
	itext t 1 "- Haskellで保証されている何らかの性質の破壊", \t -> do
	text t "* unsafeCoerceはC言語の型キャストと同じ", \t -> do
	itext t 1 "- 内部表現はそのままで型だけ変換する"
	text t "", \t -> do
	arrowIText t 0 "型と内部表現のあいだで不整合が生じ得る"
 ]

cast2 :: Page
cast2 = [\t -> do
	writeTopTitle t "cast"
	text t "", \t -> do
	text t "* unsafeCoerceをそのまま使うのは危険", \t -> do
	itext t 1 "- Haskellの持つ良い性質を捨てることになる", \t -> do
	text t "* 今やりたいことは?", \t -> do
	text t "* (SB x)で得られるxをもとの型にもどしたい", \t -> do
	itext t 1 "- 期待される型がxのもとの型と同じならJust xを", \t -> do
	itext t 1 "- そうでないならNothingを返せば良い", \t -> do
	text t "* xの型をチェック", \t -> do
	arrowIText t 1 "その型が期待されている型であればxを返し", \t -> do
	arrowIText t 1 "そうでなければNothingを返せば良い"
 ]

cast3 :: Page
cast3 = [\t -> do
	writeTopTitle t "cast"
	text t "", \t -> do
	text t "* まずは言葉通りに実装してみる", \t -> do
	itext t 1 "cast :: (Typeable a, Typeable b) => a -> Maybe b"
	itext t 1 "cast x = r"
	itext t 2 "where"
	itext t 2 "r = if typeOf x == typeOf (fromJust r)"
	itext t 3 "then Just x"
	itext t 3 "else Nothing", \t -> do
	text t "* これは型チェックを通らない", \t -> do
	itext t 1 "- aとbの型が異なる場合、then Just xが問題になる", \t -> do
	itext t 1 "- 上の場合実際にはthen部は実行されないが", \t -> do
	itext t 1 "- then部がMaybe b型でないと型の不整合が起こる"
 ]

cast4 :: Page
cast4 = [\t -> do
	writeTopTitle t "cast"
	text t "", \t -> do
	text t "* then部は型a /= 型bのときもMaybe b型である必要がある", \t -> do
	text t "* 正しい定義はこうなる", \t -> do
	itext t 1 "cast :: (Typeable a, Typeable b) => a -> Maybe b"
	itext t 1 "cast x = r"
	itext t 2 "where"
	itext t 2 "r = if typeOf x == typeOf (fromJust r)"
	itext t 3 "then Just $ unsafeCoerce x"
	itext t 3 "else Nothing", \t -> do
	text t "* 実際には実行されることのない型変換が必要になる"
 ]

fromEx :: Page
fromEx = [\t -> do
	writeTopTitle t "存在型からの値の取り出し"
	text t "", \t -> do
	text t "* 今定義したcast関数を使う", \t -> do
	text t "* Typeableクラスのインスタンスである必要がある", \t -> do
	itext t 1 "data Any = forall t . Typeable t => Any t", \t -> do
	text t "* 取り出し関数", \t -> do
	itext t 1 "fromAny :: Typeable t => Any -> Maybe t", \t -> do
	itext t 1 "fromAny (Any t) = cast t", \t -> do
	text t "* 例えばStringとして取り出す場合", \t -> do
	itext t 1 "- まず(Any t)で取り出したtは複数の型である", \t -> do
	itext t 1 "- その複数の型をcastでStringに収束させる", \t -> do
	itext t 2 "cast :: Typeable a => a -> Maybe String"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* あらゆる型からひとつの型にまとめる方法を見た", \t -> do
	text t "* その型からもとの値を取り出す方法を見た", \t -> do
	text t "* もとの値と異なる型で取り出そうとすればNothingとなる", \t -> do
	text t "* この機構は次回の「例外処理」で使われている", \t -> do
	text t "* 新たに定義された例外を自動的に一般的な例外に変換"
 ]
