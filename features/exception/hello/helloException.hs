{-# LANGUAGE DeriveDataTypeable #-}

import Control.Exception
import Data.Typeable

data MyException = ThisException | ThatException deriving (Show, Typeable)

instance Exception MyException
