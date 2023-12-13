{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Boxes where

import Control.Moffy
import Control.Moffy.Event.Time
import Control.Moffy.Samples.Event.Mouse qualified as Mouse
import Control.Moffy.Samples.Boxes.Viewable
import Data.Type.Set
import Data.Type.Flip
import Data.Or
import Data.Bool

clickOn :: Mouse.Button -> React s (Singleton Mouse.Down) ()
clickOn b = bool (clickOn b) (pure ()) . (== b) =<< Mouse.down

leftClick, middleClick, rightClick :: React s (Singleton Mouse.Down) ()
leftClick = clickOn Mouse.ButtonPrimary
middleClick = clickOn Mouse.ButtonMiddle
rightClick = clickOn Mouse.ButtonSecondary

before :: Firstable es es' a b =>
	React s es a -> React s es' b -> React s (es :+: es') Bool
a `before` b = (<$> a `first` b) \case L _ -> True; _ -> False

doubler :: React s (Mouse.Down :- Singleton TryWait) ()
doubler = do
	adjust rightClick
	r <- rightClick `before` sleep 0.2
	if r then pure () else doubler

curRect :: Point -> Sig s (Singleton Mouse.Move) Rect ()
curRect p1 = Rect p1 <$%> Mouse.position
