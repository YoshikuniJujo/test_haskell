module AtCoder.Adv20251201 where

import AtCoder.PNum

previous :: [Int] -> [Int]
previous ns = toShow (length ns) . subtract 1 $ fromShow ns
