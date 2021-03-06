module Main where

import Prelude
import Data.Identity
import Effect (Effect)
import Halogen
import Halogen.Aff
import Halogen.HTML
import Halogen.VDom.Driver

main :: Effect Unit
main = runHalogenAff do
        body <- awaitBody
        runUI followbox unit body

followbox = mkComponent {
        initialState: \_ -> {},
        render: \_ -> text "Hello, world!",
        eval: mkEval $ defaultEval { handleAction = \(Identity a) -> pure a } }
