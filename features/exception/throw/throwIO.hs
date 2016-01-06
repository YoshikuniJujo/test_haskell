import Control.Exception

catchAll :: IO a -> (SomeException -> IO a) -> IO a
catchAll = catch

hello :: a
hello = throw (ErrorCall "hello")

helloIO :: IO a
helloIO = throwIO (ErrorCall "hello")
