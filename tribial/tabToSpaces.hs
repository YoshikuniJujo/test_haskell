import qualified Data.ByteString.Char8 as BSC

main :: IO ()
main = BSC.interact $ BSC.intercalate (BSC.replicate 8 ' ') . BSC.split '\t'
