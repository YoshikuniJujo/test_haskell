import Data.Char
main=interact.map$chr.s.ord
s x|x>96=97+mod(x-78)26|0<1=65+mod(x-46)26
