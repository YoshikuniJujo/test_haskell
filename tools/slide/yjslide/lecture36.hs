import Lecture

subtitle :: String
subtitle = "第36回 OverloadedStrings拡張"

main :: IO ()
main = runLecture [
	[flip writeTitle subtitle], prelude, exampleCode, mechanism,
	integerString, integerString2, integerString3, integerString4,
	summary
 ]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* Stringを置き換えるデータ構造について学んだ", \t -> do
	text t "* それらはライブラリの形で後から追加された", \t -> do
	text t "* よって、リテラルとして表現することはできない", \t -> do
	text t "* 明示的なpackが必要になる", \t -> do
	itext t 1 "hello :: ByteString"
	itext t 1 "hello = pack \"hello\"", \t -> do
	text t "* packを書かなくてすめばより「文字列」っぽさが向上する", \t -> do
	text t "* OverloadedStrings拡張を使えば良い"
 ]

exampleCode :: Page
exampleCode = [\t -> do
	writeTopTitle t "ByteStringの例"
	text t "", \t -> do
	text t "* ByteStringの例は以下のようになる"
	itext t 1 "", \t -> do
	itext t 1 "{-# LANGUAGE OverloadedStrings #-}"
	itext t 1 ""
	itext t 1 "import qualified Data.ByteString.Char8 as BSC"
	itext t 1 ""
	itext t 1 "hello :: BSC.ByteString"
	itext t 1 "hello = \"hello\""
 ]

mechanism :: Page
mechanism = [\t -> do
	writeTopTitle t "どんな実装か"
	text t "", \t -> do
	text t "* 文字列リテラルの書かれた場所からそのタイプを知る", \t -> do
	text t "* そのタイプはIsStringのインスタンスである必要がある", \t -> do
	text t "* IsStringのメソッドであるfromStringを使って値を構築", \t -> do
	text t "* つまり、自分で使った文字列型もリテラルで表現できる"
 ]

integerString :: Page
integerString = [\t -> do
	writeTopTitle t "Integerを文字列型とする"
	text t "", \t -> do
	text t "* 実用的ではないが面白い例を作ろう", \t -> do
	text t "* Integerを文字列として扱ってみる", \t -> do
	text t "* toString関数とfromString関数を作ろう", \t -> do
	text t "* toStringはIntegerをWord8のリストにする関数を使う", \t -> do
	text t "* fromStringはWord8をIntegerのリストにする関数を使おう", \t -> do
	text t "* CharとWord8の相互変換にはord, chr, fromIntegralを使う", \t -> do
	text t "* 以下のモジュールをimportしておく", \t -> do
	itext t 1 "Data.Char, Data.Word, Data.Bits, Data.String"
 ]

integerString2 :: Page
integerString2 = [\t -> do
	writeTopTitle t "Integerを文字列型とする"
	text t "", \t -> do
	text t "* Integerと[Word8]の相互変換", \t -> do
	itext t 1 "toWord8s :: Integer -> [Word8]"
	itext t 1 "toWord8s 0 = []"
	itext t 1 "toWord8s i = fromInteger (i .&. 0xFF) :"
	itext t 4 "toWord8s (i `shiftR` 8)", \t -> do
	itext t 1 "fromWord8s :: [Word8] -> Integer"
	itext t 1 "fromWord8s [] = 0"
	itext t 1 "fromWord8s (w : ws) = toInteger w .|."
	itext t 4 "(fromWord8s ws `shiftL` 8)"
 ]

integerString3 :: Page
integerString3 = [\t -> do
	writeTopTitle t "Integerを文字列型とする"
	text t "", \t -> do
	text t "* Word8とCharの相互変換", \t -> do
	itext t 1 "word8ToChar :: Word8 -> Char"
	itext t 1 "word8ToChar = chr . fromIntegral", \t -> do
	itext t 1 "charToWord8 :: Char -> Word8"
	itext t 1 "charToWord8 = fromIntegral . ord", \t -> do
	text t "* IntegerとStringの相互変換", \t -> do
	itext t 1 "toString :: Integer -> String"
	itext t 1 "toString = map word8ToChar . toWord8s", \t -> do
	itext t 1 "instance IsString Integer where"
	itext t 2 "fromString = fromWord8s . map charToWord8"
 ]

integerString4 :: Page
integerString4 = [\t -> do
	writeTopTitle t "Integerを文字列型とする"
	text t "", \t -> do
	text t "* IsStringのfromStringメソッドを定義した", \t -> do
	itext t 1 "- fromStringは文字列リテラルの読み込みに使われる", \t -> do
	text t "* 試してみる", \t -> do
	itext t 1 "> :set -XOverloadedStrings"
	itext t 1 "> \"Hello\" :: Integer"
	itext t 1 "478560413000"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* String以外の文字列をリテラル表記したい", \t -> do
	text t "* OverloadedStrings拡張を使う", \t -> do
	text t "* IsStringクラスのインスタンスにすれば", \t -> do
	itext t 1 "- どんな型でも文字列リテラルで表現できる"
 ]
