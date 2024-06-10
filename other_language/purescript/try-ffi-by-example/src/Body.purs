module Body where

import Data.Show

data Body

foreign import body :: Body
foreign import showBody :: Body -> String

instance Show Body where show = showBody
