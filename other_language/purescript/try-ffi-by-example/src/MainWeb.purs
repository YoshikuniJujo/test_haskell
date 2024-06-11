module MainWeb where

import Prelude
import Effect
import Alert
import Body
import Window

import EventTarget

main :: Effect Unit
main = do
        alert "foobar"
        addEventListener win optionsZero
                $ CallbackFunction \(ev :: EventLoad Window) -> alert $ currentTarget ev
