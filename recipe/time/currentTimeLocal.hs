import Data.Time.LocalTime

main :: IO ()
main = do
	zt <- getZonedTime
	print zt
