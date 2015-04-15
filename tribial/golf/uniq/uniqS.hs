import Data.List
main=interact$unlines.map head.filter((>1).length).group.sort.lines
