import Control.Arrow
import Data.List

main = interact $ show . map (head &&& length) . group . sort
