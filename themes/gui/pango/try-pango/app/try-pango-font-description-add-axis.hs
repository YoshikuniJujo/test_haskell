{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Language.Haskell.TH
import Data.Font.VariationAxis

main :: IO ()
main = print =<< ppr <$> runQ (pangoFontDescriptionAddAxis "Foo" "foo")
