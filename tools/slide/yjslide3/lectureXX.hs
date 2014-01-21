import Data.Char

import Lecture

subtitle :: String
subtitle = "第11回 Either型"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, prelude2,
	aboutEither, aboutEither2, aboutEither3, aboutEither4,
	aboutMaybe
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 型aと型bとを含む型はタプル(a, b)で表現できる", \t -> do
	text t "* 型aまたは型bのどちらかであるような型は", \t -> do
	itext t 1 "Either a bで表現できる", \t -> do
	text t "* 値()のひとつだけを含む型は()で表現される", \t -> do
	text t "* 今まで見てきた型はタプルとEitherと()だけで表現可能", \t -> do
	text t "* Maybe型やリストもこれらで表現することができる", \t -> do
	text t "* 今回はEither型について見ていこう"
 ]

prelude2 :: Page
prelude2 = [\t -> do
	writeTopTitle t "()型"
	text t "", \t -> do
	text t "* ()型はユニット型と読む", \t -> do
	text t "* Either型を見る前に()型を見てみよう", \t -> do
	text t "* 例えばBool型は値を2つ持つ型であり、その値は", \t -> do
	itext t 1 "False, True", \t -> do
	text t "* 同様に()型は値を1つだけ持つ型であり、その値は", \t -> do
	itext t 1 "()", \t -> do
	text t "* 例えば()型の値は以下のように定義される", \t -> do
	itext t 1 "a :: ()"
	itext t 1 "a = ()", \t -> do
	text t "* 型の名前と値の名前が同じだが、別の物である"
 ]

aboutEither :: Page
aboutEither = [\t -> do
	writeTopTitle t "Either型"
	text t "", \t -> do
	text t "* Either Int Stringを考える", \t -> do
	text t "* この型の値は以下のようになる", \t -> do
	itext t 1 "Left 888, Left 7, Right \"hello\", Right \"yes\""
 ]

aboutEither2 :: Page
aboutEither2 = [\t -> do
	writeTopTitle t "Either型", \t -> do
	text t "* やってみよう", \t -> do
	itext t 1 "Prelude> Left 123", \t -> do
	itext t 1 $ show $ (Left 123 :: Either Int String), \t -> do
	itext t 1 "Prelude> :t it", \t -> do
	itext t 1 "it :: Either Integer b", \t -> do
	itext t 1 "Prelude> Right \"hello\"", \t -> do
	itext t 1 $ show $ (Right "hello" :: Either Int String), \t -> do
	itext t 1 "Prelude> :t it", \t -> do
	itext t 1 "it :: Either a [Char]", \t -> do
	itext t 1 "Prelude> Left 123 :: Either Int String", \t -> do
	itext t 1 $ show $ (Left 123 :: Either Int String), \t -> do
	itext t 1 "Prelude> :t it", \t -> do
	itext t 1 "it :: Either Int String"
 ]

aboutEither3 :: Page
aboutEither3 = [\t -> do
	writeTopTitle t "Either型"
	text t "", \t -> do
	text t "* ある文字列について", \t -> do
	itext t 1 "- すべての文字が数字ならそれが表す数を", \t -> do
	itext t 1 "- そうでなければ文字列それ自体を返す関数を書こう", \t -> do
	text t "* all :: (a -> Bool) -> [a] -> Boolという関数があり", \t -> do
	itext t 1 "リストのすべてが条件を満たすかどうかをチェックする", \t -> do
	text t "* Data.Charモジュールの関数isDigitは", \t -> do
	itext t 1 "ある文字が数字かどうかを調べる", \t -> do
	text t "* read関数は文字列を読みそれを数として出力する"
 ]

readInt :: String -> Either Int String
readInt str
	| all isDigit str = Left $ read str
	| otherwise = Right str

aboutEither4 :: Page
aboutEither4 = [\t -> do
	writeTopTitle t "Either型"
	text t "", \t -> do
	text t "* これらを使うと", \t -> do
	itext t 1 "readInt :: String -> Either Int String", \t -> do
	itext t 1 "readInt str", \t -> do
	itext t 2 "| all isDigit str = Left $ read str"
	itext t 2 "| otherwise = Right str", \t -> do
	text t "* これをlectures/lecture11/either.hsに書き込み", \t -> do
	text t "* ghci either.hsで読み込もう", \t -> do
	itext t 1 "*Main> readInt \"132\"", \t -> do
	itext t 1 $ show $ readInt "132", \t -> do
	itext t 1 "*Main> readInt \"13 dogs\"", \t -> do
	itext t 1 $ show $ readInt "13 dogs"
 ]

aboutMaybe :: Page
aboutMaybe = [\t -> do
	writeTopTitle t "Maybe型的なもの"
	text t "", \t -> do
	text t "* Maybe a型とは以下の値を含む型なので", \t -> do
	itext t 1 "- a型の値または", \t -> do
	itext t 1 "- 失敗を表す単一の値", \t -> do
	text t "* 同等のものがEitherを使って以下のように作れる", \t -> do
	itext t 1 "Either () a", \t -> do
	text t "* Maybe aとEither () aのそれぞれの対応はこうなる", \t -> do
	itext t 1 "Nothing"
	preLine t
	itext t 4 ": Left ()", \t -> do
	itext t 1 "Just [a型の値]"
	preLine t
	itext t 4 ": Right [a型の値]"
 ]
