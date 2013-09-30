module Main where

import Lecture

main :: IO ()
main = runLecture pages

subtitle :: String
subtitle = "第3回 本質的な意味論と構文糖"

pages :: [Page]
pages = [
	titlePage,
	minimal5,
	minimal5' 1, syntaxDefinition,
	minimal5' 2, syntaxLambda,
	minimal5' 3, syntaxApply,
	minimal5' 4, syntaxData, syntaxData2,
	minimal5' 5, syntaxCase,
	funDef, multiArgFun, operator, list, list2, string, tuple,
	summary,
	typeDef1, typeDef2
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

min5, min5' :: [String]
min5 = [
	"[変数] = [式]",
	"\\[引数] -> [式]",
	"[関数名] [引数]",
	"data [型名] = [構築子] [型] ... | ...",
	"case [式] of [パターン] -> [式]; ..."
 ]
min5' = [
	"変数の定義",
	"関数リテラル",
	"関数適用",
	"data構文",
	"case式"
 ]

minimal5 :: Page
minimal5 = [\t -> do
	writeTopTitle t "最小限の構文"
	text t "基本となる最小限の5つの構文を示す"
	text t ""
 ] ++ zipWith (\n s -> \t -> semititle t $ show n ++ ". " ++ s) [1 :: Int ..] min5'

minimal5' :: Int -> Page
minimal5' n = [\t -> do
	writeTopTitle t "最小限の構文"
	oneshot t $ do
		text t "基本となる最小限の5つの構文を示す"
		text t ""
		writeNRed t 1 min5'
 ]	where
	writeNRed _ _ [] = return ()
	writeNRed t i (s : ss) = do
		(if i == n then withRed t else id) $ semititle t $
			show i ++ ". " ++ s
		writeNRed t (succ i) ss

syntaxData :: Page
syntaxData = [\t -> do
	writeTopTitle t "data構文"
	text t "", \t -> do
	semititle t "data [型名] = [構築子] [型] ... | ..."
	text t "", \t -> do
	text t "例1: data Bool = False | True", \t -> do
	text t "例2: data Shape"
	itext t 1 "= Rectangle Double Double"
	itext t 1 "| Circle Double"
	text t "", \t -> do
	text t "* 列挙することによって0から型を作る", \t -> do
	text t "* 既存の型を組み合わせて新しい型を作る"
 ]

syntaxData2 :: Page
syntaxData2 = [\t -> do
	writeTopTitle t "data構文"
	text t "型変数を使えば複数の型をまとめて定義可能"
	text t "", \t -> do
	text t "例: data Maybe a = Just a | Nothing"
	text t "", \t -> do
	text t "* これは以下のような定義をまとめたもの"
	itext t 1 "data Maybe Int = Just Int | Nothing"
	itext t 1 "data Maybe String = Just String | Nothing"
	itext t 1 "data Maybe Foo = Just Foo | Nothing"
	itext t 2 "..."
 ]

syntaxDefinition :: Page
syntaxDefinition = [\t -> do
	writeTopTitle t "変数の定義"
	text t "", \t -> do
	semititle t "[変数] = [式]"
	text t "", \t -> do
	text t "例1: three = 3", \t -> do
	text t "例2: six = three + three"
	text t "", \t -> do
	text t "* 変数に値を束縛する"
	text t "* 左辺は変数名で右辺は任意の式"
 ]

syntaxLambda :: Page
syntaxLambda = [\t -> do
	writeTopTitle t "関数リテラル"
	text t "", \t -> do
	semititle t "\\[引数] -> [式]"
	text t "", \t -> do
	text t "例1: \\x -> f x", \t -> do
	text t "例2: \\x -> x * x"
	text t "", \t -> do
	text t "* \\に続けて引数名を置く", \t -> do
	text t "* ->の後に式を書く", \t -> do
	text t "* 例1はxを入力としf xを出力とする関数(実はfと同じ)", \t -> do
	text t "* 例2はxを入力としx * xを出力とする関数"
 ]

syntaxCase :: Page
syntaxCase = [\t -> do
	writeTopTitle t "case 式"
	text t "", \t -> do
	semititle t "case [式] of [パターン] -> [式]; ..."
	text t "", \t -> do
	text t "例: case lookup word wordCounts of"
	itext t 1 "Just c -> c"
	itext t 1 "Nothing -> 0"
 ]

syntaxApply :: Page
syntaxApply = [\t -> do
	writeTopTitle t "関数適用"
	text t "", \t -> do
	semititle t "[関数名] [引数]"
	text t "", \t -> do
	text t "例: fun x"
	text t "", \t -> do
	text t "* 関数名に続けて引数を書く", \t -> do
	text t "* 空白以外の()等は不要"
 ]

funDef :: Page
funDef = [\t -> do
	writeTopTitle t "関数定義"
	text t "関数定義は実は構文糖である"
	text t "", \t -> do
	text t "例1: fun x = x * x", \t -> do
	arrowIText t 1 "fun = \\x -> x * x"
	text t "", \t -> do
	text t "例2: fun (Just n) = n * n"
	itext t 0.85 "fun Nothing = 0", \t -> do
	arrowIText t 1 "fun = \\x -> case x of"
	itext t 2 "Just n -> n * n"
	itext t 2 "Nothing -> 0"
 ]

multiArgFun :: Page
multiArgFun = [\t -> do
	writeTopTitle t "複数の入力を持つ関数"
	text t "Haskellには複数の入力を持つ関数はない"
	text t "", \t -> do
	semititle t "じゃあ add 3 8 は?", \t -> do
	text t "(add 3) 8 ということ", \t -> do
	text t "* addという関数は引数xを取り、"
	itext t 1 "「引数yを取りx+yを返す関数」を返す関数"
 ]

operator :: Page
operator = [\t -> do
	writeTopTitle t "演算子"
	text t "Haskellでは演算子と関数の違いは「見た目」だけ"
	text t "", \t -> do
	text t "* add 3 4 は 3 `add` 4 と書ける", \t -> do
	text t "* 3 + 4 は (+) 3 4 と書ける"
	text t "", \t -> do
	writeNextTitle t "型構築演算子"
	text t ":で始まる演算子は型構築子である"
	text t "", \t -> do
	text t "例: data Addition = Int :+: Int"
 ]

list :: Page
list = [\t -> do
	writeTopTitle t "リスト"
	text t "リストとはCで言うところのリンクトリストのようなもの", \t -> do
	text t "Haskell では以下のように定義することが可能"
	text t "", \t -> do
	text t "data List a = List a (List a) | Empty"
	text t "", \t -> do
	text t "もっと見やすくするために型構築演算子を使えばこうなる"
	text t ""
	text t "data List a = a ::: List a | Empty"
	text t "", \t -> do
	text t "* 型aのリストは型aのリストの頭に型aの値を足したもの"
	itext t 1 "または空リスト"
 ]

list2 :: Page
list2 = [\t -> do
	writeTopTitle t "リスト"
	text t "他の言語でループを使うような場面で"
	itext t 1 "Haskellではリストを使うことが多い"
	arrowIText t 1 "リストには特別な構文糖が用意されている"
	text t "", \t -> do
	text t "組み込みのリストの定義"
	text t "data [] a = a : ([] a) | []"
	text t "", \t -> do
	text t "1, 2, 3をメンバーとするリストは以下のように書ける"
	text t "1 : 2 : 3 : []", \t -> do
	itext t 1 "構文糖を使うと"
	preLine t
	arrowIText t 4 "[1, 2, 3]"
 ]

string :: Page
string = [\t -> do
	writeTopTitle t "文字列"
	text t "文字列はHaskellでは単なるCharのリスト"
	text t "", \t -> do
	text t "['h', 'e', 'l', 'l', 'o']のように書ける", \t -> do
	itext t 1 "ちょっとめんどくさい"
	dvArrow t
	preLine t
	itext t 3 "(構文糖)"
	itext t 2 "\"hello\"", \t -> do
	text t "つまり"
	itext t 1 "\"hello\""
	arrowIText t 1 "['h', 'e', 'l', 'l', 'o']" 
	arrowIText t 1 "'h' : 'e' : 'l' : 'l' : 'o' : []"
	text t "ということ"
 ]

tuple :: Page
tuple = [\t -> do
	writeTopTitle t "タプル"
	text t "リストと同様にタプルも文法的に特別扱いされている"
	text t "", \t -> do
	text t "組み込みのタプルは以下のように定義される"
	text t "(,) a b = (,) a b"
	text t "(,,) a b c = (,,) a b c"
	text t "(,,,) a b c d = (,,,) a b c d"
	itext t 1 "..."
	text t "", \t -> do
	itext t 1 "(,,) 1 2 3", \t -> do
	dvArrow t
	preLine t
	itext t 3 "構文糖"
	itext t 1 "(1, 2, 3)"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* Haskellは小さな構文群に砂糖を少々加えたもの", \t -> do
	text t "* 本質的に必要な構文には他に型クラスに関するものがある", \t -> do
	text t "* 多くの型は効率等を考慮しなければこの枠組で定義可能", \t -> do
	text t "* IO型はこの枠組内で定義することができない"
 ]

typeDef1 :: Page
typeDef1 = [\t -> do
	writeTopTitle t "追加: 型宣言"
	text t "", \t -> do
	text t "* 今までの例では型宣言をしてこなかった", \t -> do
	text t "* ほとんどの場合Haskellでは型推論が利く"
	text t "", \t -> do
	semititle t "しかし、型宣言はしたほうがいい", \t -> do
	text t "* 関数の中身を考える前に型を考える", \t -> do
	itext t 1 "- 入力と出力の範囲をまずは明確にする"
	itext t 1 "- 頭を整理することができる"
	itext t 1 "- 自分が何をしようとしているのかが明確になる", \t -> do
	text t "* 型宣言はドキュメントとして優れる", \t -> do
	itext t 1 "- 与えられるものと得られるものの種類がわかる"
	itext t 1 "- 実際のコードとの齟齬がありえない"
 ]

typeDef2 :: Page
typeDef2 = [\t -> do
	writeTopTitle t "追加: 型宣言"
	text t "", \t -> do
	semititle t "[変数] :: [型]"
	text t "", \t -> do
	text t "* 型は型定数や型変数を型適用で結合したもの"
	itext t 1 "Maybe a や [] Int 等", \t -> do
	text t "* 関数の型は[入力型] -> [出力型]で表される"
	text t "", \t -> do
	text t "例: Int型のリストを取りString型の値を返す関数something"
	itext t 1 "something :: [Int] -> String"
	text t "", \t -> do
	text t "型に関してもリストとタプルは特別扱いで例えば、"
	itext t 1 "[Int], (Int, Bool, String)などと書ける"
 ]
