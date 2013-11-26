import Lecture

subtitle :: String
subtitle = "第25回 入出力例外"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage, prelude,
	useReadFile, useReadFile2, useReadFile3, useReadFile4,
	useReadFile5, useReadFile6, useReadFile7,
	useReadFile8, useReadFile9, useReadFile10,
	useReadFile11, useReadFile12, useReadFile13, useReadFile14, useReadFile15,
	summary
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]

prelude :: Page
prelude = [\t -> do
	writeTopTitle t "はじめに"
	text t "", \t -> do
	text t "* 前回までで例外処理の仕組みはだいたい学べた", \t -> do
	text t "* 今回は入出力例外にしぼって見ていく", \t -> do
	text t "* 歴史的な理由で以下のようになっている", \t -> do
	itext t 1 "type IOError = IOException", \t -> do
	text t "* Haskell 2010ではIOErrorとして定義されている", \t -> do
	text t "* GHCではHaskell 2010にはない種類のIOErrorがある", \t -> do
	text t "* IOErrorの種類や作りかた、情報の引き出しかた等を学ぶ", \t -> do
	text t "* 例を挙げながら説明していく"
 ]

useReadFile :: Page
useReadFile = [\t -> do
	writeTopTitle t "readFile"
	text t "", \t -> do
	text t "* ファイルの読み込み時に考えられる例外", \t ->
	itext t 1 "- 指定されたファイルが存在しない", \t -> do
	itext t 1 "- ファイルの読み込み権限がない", \t -> do
	itext t 1 "- 書き込みモードでファイルがすでに開かれている", \t -> do
	itext t 1 "- 文字コードの変換の失敗", \t -> do
	text t "* それぞれの例外について以下を試してみよう", \t -> do
	itext t 1 "- 発生させる", \t -> do
	itext t 1 "- 捕捉する", \t -> do
	itext t 1 "- 情報を取得する"
 ]

useReadFile2 :: Page
useReadFile2 = [\t -> do
	writeTopTitle t "ファイルが存在しない"
	text t "", \t -> do
	text t "* 例外を発生させてみよう", \t -> do
	itext t 0 "> readFile \"notExist.txt\""
	itext t 0 "*** Exception: notExist.txt: openFile:"
	itext t 1 "does notExist (No such file or directory)", \t -> do
	text t "* 例外を捕捉してみる", \t -> do
	itext t 0 "> tryJust"
	itext t 1 "(\\e -> guard (isDoesNotExistError e) >> return e) $"
	itext t 1 "readFile \"notExist.txt\""
	itext t 0 "Left notExist.txt: openFile:"
	itext t 1 "does not exist (No such file or directory)"
 ]

useReadFile3 :: Page
useReadFile3 = [\t -> do
	writeTopTitle t "ファイルが存在しない"
	text t "", \t -> do
	text t "* 例外からいろいろな情報を引き出す", \t -> do
	itext t 1 "getNEInfo ::"
	itext t 2 "(IOError -> a) -> IO (Either a String)"
	itext t 1 "getNEInfo info = tryJust"
	itext t 2 "(\\e -> guqrd (isDoesNotExistError e) >>"
	itext t 3 "return (info e)) $"
	itext t 2 "readFile \"notExist.txt\"", \t -> do
	itext t 1 "> getNEInfo ioeGetErrorType"
	itext t 1 "Left does not exist"
 ]

useReadFile4 :: Page
useReadFile4 = [\t -> do
	writeTopTitle t "ファイルが存在しない"
	text t "", \t -> do
	itext t 1 "> getNEInfo ioeGetLocation"
	itext t 1 "Left \"openFile\"", \t -> do
	itext t 1 "> getNEInfo ioeGetErrorString"
	itext t 1 "Left \"does not exist\"", \t -> do
	itext t 1 "> getNEInfo ioeGetHandle"
	itext t 1 "Left Nothing", \t -> do
	itext t 1 "> getNEInfo ioeGetFileName"
	itext t 1 "Left (Just \"notExist.txt\")"
 ]

useReadFile5 :: Page
useReadFile5 = [\t -> do
	writeTopTitle t "読み込み権限がない"
	text t "", \t -> do
	text t "* 例外を発生させる", \t -> do
	itext t 0 "> readFile \"/etc/shadow\""
	itext t 0 "*** Exception: /etc/shadow: openFile:"
	itext t 1 "permission denied (Permission denied)", \t -> do
	text t "* 例外を捕捉する", \t -> do
	itext t 0 "> tryJust"
	itext t 1 "(\\e -> guqrd (isPermissionError e) >> return e) $"
	itext t 1 "readFile \"/etc/shadow\""
	itext t 0 "Left /etc/shadow: openFile:"
	itext t 1 "permission denied (Permission denied)"
 ]

useReadFile6 :: Page
useReadFile6 = [\t -> do
	writeTopTitle t "読み込み権限がない"
	text t "", \t -> do
	text t "* いろいろな情報を引き出す", \t -> do
	itext t 1 "getPEInfo"
	itext t 2 ":: (IOError -> a) -> IO (Either a String)"
	itext t 1 "getPEInfo info = tryJust"
	itext t 2 "(\\e -> guard (isPermissionError e) >>"
	itext t 3 "return (info e)) $"
	itext t 2 "readFile \"/etc/shadow\"", \t -> do
	itext t 1 "> getPEInfo ioeGetErrorType"
	itext t 1 "Left permission denied"
 ]

useReadFile7 :: Page
useReadFile7 = [\t -> do
	writeTopTitle t "読み込み権限がない"
	text t "", \t -> do
	itext t 1 "> getPEInfo ioeGetLocation"
	itext t 1 "Left \"openFile\"", \t -> do
	itext t 1 "> getPEInfo ioeGetErrorString"
	itext t 1 "Left \"permission denied\"", \t -> do
	itext t 1 "> getPEInfo ioeGetHandle"
	itext t 1 "Left Nothing", \t -> do
	itext t 1 "> getPEInfo ioeGetFileName"
	itext t 1 "Left (Just \"/etc/shadow\")"
 ]

useReadFile8 :: Page
useReadFile8 = [\t -> do
	writeTopTitle t "すでに開かれている"
	text t "", \t -> do
	text t "* 例外を発生させてみる", \t -> do
	itext t 0 "> openFile \"test.txt\" WriteMode"
	itext t 0 "> readFile \"test.txt\""
	itext t 0 "*** Exception: test.txt: openFile:"
	itext t 1 "resource busy (file is locked)", \t -> do
	text t "* 例外を捕捉してみる", \t -> do
	itext t 0 "> tryJust"
	itext t 1 "(\\e -> guard (isAlreadyInUseError e) >> return e) $"
	itext t 1 "readFile \"test.txt\""
	itext t 0 "Left test.txt: openFile: resource busy (file is locked)"
 ]

useReadFile9 :: Page
useReadFile9 = [\t -> do
	writeTopTitle t "すでに開かれている"
	text t "", \t -> do
	text t "* いろいろな情報を取得", \t -> do
	itext t 1 "getAIUInfo"
	itext t 2 ":: (IOError -> a) -> IO (Either a String)"
	itext t 1 "getPEInfo info = tryJust"
	itext t 2 "(\\e -> guard (isAlreadyInUseError e) >>"
	itext t 3 "return (info e)) $"
	itext t 2 "readFile \"test.txt\"", \t -> do
	itext t 1 "> openFile \"test.txt\" WriteMode"
	itext t 1 "> getAIUInfo ioeGetErrorType"
	itext t 1 "Left resource busy"
 ]

useReadFile10 :: Page
useReadFile10 = [\t -> do
	writeTopTitle t "すでに開かれている"
	text t "", \t -> do
	itext t 1 "> getAIUInfo ioeGetLocation"
	itext t 1 "Left \"openFile\"", \t -> do
	itext t 1 "> getAIUInfo ioeGetErrorString"
	itext t 1 "Left \"resource busy\"", \t -> do
	itext t 1 "> getAIUInfo ioeGetHandle"
	itext t 1 "Left Nothing", \t -> do
	itext t 1 "> getAIUInfo ioeGetFileName"
	itext t 1 "Left (Just \"test.txt\")"
 ]

useReadFile11 :: Page
useReadFile11 = [\t -> do
	writeTopTitle t "文字コードの変換の失敗"
	text t "", \t -> do
	text t "* 例外を発生させてみる", \t -> do
	text t "> openBinaryFile \"bad.txt\" ReadMode"
	text t "{handle: bad.txt}"
	text t "> hGetContents it"
	text t "\"\\194\"", \t -> do
	text t "> localeEncoding"
	text t "UTF-8", \t -> do
	text t "> readFile \"bad.txt\""
	text t "*** Exception: bad.txt: hGetContents:"
	itext t 1 "invalid argument (invalid byte sequence)"
 ]

useReadFile12 :: Page
useReadFile12 = [\t -> do
	writeTopTitle t "文字コードの変換の失敗"
	text t "", \t -> do
	text t "* isInvalidArgumentを作る", \t -> do
	text t "isInvalidArgument IOError { ioe_type = InvalidArgument } = True"
	text t "isInvalidArgument _ = False", \t -> do
	text t "* 例外を捕捉してみる", \t -> do
	text t "> tryJust"
	itext t 1 "(\\e -> guard (isInvalidArgument e) >> return e) $"
	itext t 1 "readFile \"bad.txt\""
	text t "Right \"*** Exception: bad.txt: hGetContents: ...", \t -> do
	text t "* 捕捉に失敗している", \t -> do
	text t "* readFileが遅延IOを行うため", \t -> do
	itext t 1 "- 例外は'Right \"'の次の文字の表示で生じる"
 ]

useReadFile13 :: Page
useReadFile13 = [\t -> do
	writeTopTitle t "文字コードの変換の失敗"
	text t "", \t -> do
	text t "* evaluateしてやれば良い", \t -> do
	text t "> tryJust"
	itext t 1 "(\\e -> guard (isInvalidArgument e) >> return e) $"
	itext t 1 "readFile \"bad.txt\" >>= evaluate"
	text t "Left bad.txt: hGetContents:"
	itext t 1 "invalid argument (invalid byte sequence)"
 ]

useReadFile14 :: Page
useReadFile14 = [\t -> do
	writeTopTitle t "文字コードの変換の失敗"
	text t "", \t -> do
	text t "* いろいろな情報の取得", \t -> do
	itext t 1 "getIAInfo ::"
	itext t 2 "(IOError -> a) -> IO (Either a String)"
	itext t 1 "getIAInfo info = tryJust"
	itext t 2 "(\\e -> guard (isInvalidArgument e) >>"
	itext t 3 "return (info e)) $"
	itext t 2 "readFile \"bad.txt\" >>= evaluate", \t -> do
	itext t 1 "> getIAInfo ioeGetErrorType"
	itext t 1 "Left invalid argument"
 ]

useReadFile15 :: Page
useReadFile15 = [\t -> do
	writeTopTitle t "文字コードの変換の失敗"
	text t "", \t -> do
	itext t 1 "> getIAInfo ioeGetLocation"
	itext t 1 "Left \"hGetContents\"", \t -> do
	itext t 1 "> getIAInfo ioeGetErrorString"
	itext t 1 "Left \"invalid argument\"", \t -> do
	itext t 1 "> getIAInfo ioeGetHandle"
	itext t 1 "Left (Just {handle: bad.txt})", \t -> do
	itext t 1 "> getIAInfo ioeGetFileName"
	itext t 1 "Left (Just \"bad.txt\")"
 ]

summary :: Page
summary = [\t -> do
	writeTopTitle t "まとめ"
	text t "", \t -> do
	text t "* IOErrorを捕捉し、そこからいろいろな情報を取得した", \t -> do
	text t "* GHCはIOErrorの種類やそれが含む情報を拡張している", \t -> do
	text t "* readFileを例に4種のIOErrorについて見てみた"
 ]
