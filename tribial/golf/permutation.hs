import Data.List
main=interact$unlines.sort.permutations.head.lines
