{-# LANGUAGE BlockArguments #-}

module Main where

import Foreign.C.String

main :: IO ()
main = cPrintfS "あいうえお\n"

cPrintfS :: String -> IO ()
cPrintfS s = withCString "%s" \cf -> withCString s \cs -> c_printf_s cf cs

foreign import ccall "printf" c_printf_s :: CString -> CString -> IO ()
