import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable

data Human

foreign import ccall "human.h get_tarou" c_getTarou :: Ptr Human

main :: IO ()
main = do
	let t = c_getTarou
	age <- peekByteOff t 0 :: IO CInt
	height <- peekByteOff t 4 :: IO CDouble
	weight <- peekByteOff t 12 :: IO CDouble
	putStrLn $ "太郎 " ++ show age ++ "歳 " ++ show height ++ "cm " ++
		show weight ++ "kg"
