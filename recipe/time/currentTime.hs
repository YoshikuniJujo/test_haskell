-- time
import Data.Time (getCurrentTime)

main :: IO ()
main = do
	ct <- getCurrentTime
	print ct
