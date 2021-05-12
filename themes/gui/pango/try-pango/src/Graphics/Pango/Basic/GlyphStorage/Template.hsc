{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Graphics.Pango.Basic.GlyphStorage.Template where

import Foreign.ForeignPtr
import Language.Haskell.TH

mkNewtype :: String -> DecQ
mkNewtype t = newtypeD (cxt []) (mkName t) [] Nothing (normalC (mkName $ t ++ "_") [
	bangType
		(bang noSourceUnpackedness noSourceStrictness)
		(conT ''ForeignPtr `appT` conT (mkName t))
	]) []
