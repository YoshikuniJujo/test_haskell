import Foreign.C.Types
import Foreign.Ptr
import Foreign.Storable
import Foreign.Marshal.Array
import Control.Monad

foreign import ccall "mk_graph" c_mkGraph :: Ptr CInt -> CInt -> IO ()

main :: IO ()
main = allocaArray 9 $ \ptr -> do
	forM_ (zip [0 ..] [2, 3, 7, 13, 18, 21, 26, 29, 25, 17, 10, 5]) $ \(i, d) ->
		pokeElemOff ptr i d
	c_mkGraph ptr 12
