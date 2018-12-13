import Data.Char
import Numeric
main=interact$(>>= (map toUpper.c.(`showHex`"").read)).lines
c s=replicate(2-length s)'0'++s
