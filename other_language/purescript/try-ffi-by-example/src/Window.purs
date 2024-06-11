module Window where

import Data.Show

data Window

foreign import win :: Window
foreign import showWindow :: Window -> String

instance Show Window where show = showWindow
