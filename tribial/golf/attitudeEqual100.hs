import Data.Char
main=interact$show.sum.map((\x->x-64).ord.toUpper).filter isAlpha
