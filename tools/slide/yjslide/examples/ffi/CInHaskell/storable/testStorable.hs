import Storable
import Foreign.Marshal
import Foreign.Storable

testStorable :: Storable a => a -> IO a
testStorable x = alloca $ \ptr -> do
	poke ptr x
	peek ptr
