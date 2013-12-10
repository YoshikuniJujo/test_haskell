import Foreign.C.Types

foreign import ccall "add3.h add3" c_add3 :: CInt -> CInt

main :: IO ()
main = print $ c_add3 7
