{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Counter where

import Prelude hiding (scanl, repeat, break)

import Data.Type.Flip
import Data.Type.Set
import Data.OneOrMore

import MonadicFrp
import MonadicFrp.Handle
import MonadicFrp.Event.Mouse

import Field
import MonadicFrp.Run
import MonadicFrp.XFieldHandle.Mouse

data Counter = CountUp | GetCount deriving (Show, Eq, Ord)
numbered 9 [t| Counter |]
instance Request Counter where data Occurred Counter = OccCounter deriving Show

{-# ANN countUp "HLInt: ignore Use const" #-}

countUp :: React (Singleton Counter) ()
countUp = await CountUp \_ -> ()

{-# ANN getCount "HLint: ignore Use const" #-}

getCount :: React (Singleton Counter) ()
getCount = await GetCount \_ -> ()

tryCounter :: Sig (Singleton Counter) Int ()
tryCounter = scanl (+) 0 $ waitFor getCount >> emit 1

handleCounter :: Monad m => Handle' m (Singleton Counter)
handleCounter reqs = pure $ case extract reqs of
	CountUp -> Just $ singleton OccCounter
	GetCount -> Nothing

onRightClick :: React (MouseDown :- Counter :- 'Nil) ()
onRightClick = do
	adjust rightClick
	adjust countUp

action :: Sig (MouseEv :+: Counter :- 'Nil) Int ()
action = (\c _ _ -> c) <$%> scanl (+) 0 (repeat (adjust getCount >> pure 1)) <*%> repeat (adjust onRightClick) <*%> repeat (adjust leftClick)

action' :: Sig (MouseEv :+: Counter :- 'Nil) Int ()
action' = () <$ action `break` deleteEvent

tryAction :: IO ()
tryAction = do
	f <- openField "hello" [buttonPressMask]
	interpret (retry $ handleCounter `before` handleMouse Nothing f) print action'
	closeField f
