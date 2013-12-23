import Paths_library

getData = getDataFileName "hoge.txt" >>= readFile
hoge = 88

main = getData >>= putStr
