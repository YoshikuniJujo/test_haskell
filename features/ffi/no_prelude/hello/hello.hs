{-# LANGUAGE ForeignFunctionInterface #-}

foreign import ccall "hello" c_hello :: IO ()

main = c_hello
