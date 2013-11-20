import System.IO.Unsafe
import Data.IORef

coerce :: a -> b
coerce a = let ref = unsafePerformIO (newIORef undefined) in
	unsafePerformIO $ writeIORef ref a >> readIORef ref
