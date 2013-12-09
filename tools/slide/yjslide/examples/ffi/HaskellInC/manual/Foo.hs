module Foo where

import Foreign.C.Types

foreign export ccall foo :: CInt -> IO CInt

foo :: CInt -> IO CInt
foo n = return $ fromIntegral $ length $ f n

f :: CInt -> [CInt]
f 0 = []
f n = n : f (n - 1)
