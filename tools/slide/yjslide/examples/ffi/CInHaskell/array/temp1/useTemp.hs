import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Control.Monad

foreign import ccall "temp.h get_temps" c_getTemps :: Ptr CInt

showSp :: Show a => Int -> a -> String
showSp n x = let s = show x in replicate (n - length s) ' ' ++ s

main :: IO ()
main = do
	forM_ [0 .. 11] $ \i -> do
		tmp <- peekElemOff c_getTemps i
		putStrLn $ showSp 2 (i + 1) ++ "月 " ++
			init (showSp 3 tmp) ++ "." ++ [last (show tmp)]
