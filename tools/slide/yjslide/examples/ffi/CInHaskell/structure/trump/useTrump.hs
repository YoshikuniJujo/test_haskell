import Foreign.C.Types
import Foreign.Ptr
import Foreign.Marshal
import Foreign.Storable

data Card

foreign import ccall "trump.h print_card" c_printCard :: Ptr Card -> IO ()

main :: IO ()
main = allocaBytes 4 $ \pc -> do
	pokeByteOff pc 0 (2 :: CInt)
	pokeByteOff pc 4 (11 :: CInt)
	c_printCard pc
