{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Control.Moffy.Event.CalcTextExtents.DefaultWindow (
	Cte.CalcTextExtents, Cte.TextExtents(..), Cte.Rectangle(..), Cte.FontName, Cte.FontSize,
	calcTextExtents
	) where

import Control.Moffy
import Control.Moffy.Event.DefaultWindow
import Data.Type.Set

import qualified Data.Text as T
import qualified Control.Moffy.Event.CalcTextExtents as Cte

calcTextExtents :: Cte.FontName -> Cte.FontSize -> T.Text ->
	React s (LoadDefaultWindow :- Cte.CalcTextExtents :- 'Nil) Cte.TextExtents
calcTextExtents fn fs t = adjust loadDefaultWindow >>= \wid -> adjust $ Cte.calcTextExtents wid fn fs t
