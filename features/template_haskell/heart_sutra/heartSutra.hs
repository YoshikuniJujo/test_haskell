{-# LANGUAGE TemplateHaskell #-}

import Control.Monad.IO.Class
import Language.Haskell.TH

main :: IO ()
main = putStr $(litE . stringL =<< liftIO (readFile "heartSutra.txt"))
