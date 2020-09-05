{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Handle.XField.CalcTextExtents where

import Control.Moffy.Event.CalcTextExtents
import Control.Moffy.Handle
import Data.Type.Set
import Data.OneOrMore

import qualified Data.Text as T
import qualified Graphics.X11.Xrender as X

import Field

handleCalcTextExtents :: Field -> Handle IO (Singleton CalcTextExtents)
handleCalcTextExtents f (Singleton (CalcTextExtentsReq fn fs t)) = Singleton
	. OccCalcTextExtents fn fs t . xGlyphInfoToNew <$> textExtents f fn fs (T.unpack t)

xGlyphInfoToNew :: X.XGlyphInfo -> TextExtents
xGlyphInfoToNew (X.XGlyphInfo w_ h_ x_ y_ xo_ yo_) = TextExtents x y w h xo yo
	where [w, h, x, y, xo, yo] = fromIntegral <$> [w_, h_, x_, y_, xo_, yo_]
