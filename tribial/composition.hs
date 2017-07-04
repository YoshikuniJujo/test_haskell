import Control.Category

f = lines >>> zip (map show [1..]) >>> map (uncurry (++)) >>> unlines
