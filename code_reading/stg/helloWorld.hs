foreign import ccall "hello" c_hello :: IO ()
foreign import ccall "world" c_world :: IO ()

main = c_hello >> c_world
