import Control.Applicative
import Data.Tree

bfs = (map rootLabel <$>)
	. takeWhile (not . null) . iterate (>>= subForest) . (: [])
