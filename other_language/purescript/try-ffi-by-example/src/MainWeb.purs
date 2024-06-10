module MainWeb where

import Prelude
import Effect
import Alert
import Body

import EventTarget

main :: Effect Unit
main = do
        alert "foobar"
        addEventListener body optionsZero
                $ CallbackFunction \(ev :: EventLoad) -> alert "body loaded"
