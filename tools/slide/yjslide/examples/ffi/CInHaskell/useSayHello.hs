import Foreign.C.Types

foreign import ccall "sayHello" c_sayHello :: IO ()

main :: IO ()
main = c_sayHello
