{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

import Language.Haskell.TH
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Variations

main :: IO ()
main = print =<< ppr <$> runQ (pangoFontDescriptionAddAxis "Foo" "foo")
