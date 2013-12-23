module Library where

import Paths_library

getData = getDataFileName "hoge.txt" >>= readFile
hoge = 88
