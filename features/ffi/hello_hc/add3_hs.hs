import Foreign.C.Types

foreign import ccall unsafe "add3.h add3" c_add3 :: CInt -> CInt

add3 :: Int -> Int
add3 = fromIntegral . c_add3 . fromIntegral

main :: IO ()
main = print $ add3 8
