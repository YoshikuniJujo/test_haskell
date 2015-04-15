import Data.Char
main=interact$map(chr.s.ord)
s x=if x>96 then 97+mod(x-78)26 else 65+mod(x-46)26
