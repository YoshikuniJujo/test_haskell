{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Concurrent.STM (atomically, TVar, newTVar, readTVar, modifyTVar)
import Control.Moffy
import Control.Moffy.Handle
import Control.Moffy.Run
import Data.Type.Set
import Data.OneOrMore qualified as Oom
import Data.OneOrMoreApp qualified as App
import Data.Map qualified as M
import Data.Bool

type Point = (Double, Double)

data SetArea = SetAreaReq Int Point Point deriving (Show, Eq, Ord)
numbered [t| SetArea |]
instance Request SetArea where data Occurred SetArea = OccSetArea deriving Show

setArea :: Int -> (Point, Point) -> React s (Singleton SetArea) ()
setArea i (lu, rd) = await (SetAreaReq i lu rd) $ const ()

data GetArea = GetAreaReq Int deriving (Show, Eq, Ord)
numbered [t| GetArea |]

instance Request GetArea where
	data Occurred GetArea = OccGetArea Int Point Point deriving Show

getArea :: Int -> React s (Singleton GetArea) (Point, Point)
getArea i0 = go >>= \(i, a) -> bool (getArea i0) (pure a) (i == i0)
	where
	go = await (GetAreaReq i0) \(OccGetArea i lu rd) -> (i, (lu, rd))

handle :: TVar (M.Map Int (Point, Point)) -> Handle' IO (SetArea :- Singleton GetArea)
handle vm = handleSetArea vm `merge` handleGetArea vm

handleSetArea :: TVar (M.Map Int (Point, Point)) -> Handle' IO (Singleton SetArea)
handleSetArea vm (Oom.Singleton (SetAreaReq i ul dr)) = do
	atomically . modifyTVar vm $ M.insert i (ul, dr)
	pure . Just $ App.Singleton OccSetArea

handleGetArea :: TVar (M.Map Int (Point, Point)) -> Handle' IO (Singleton GetArea)
handleGetArea vm (Oom.Singleton (GetAreaReq i)) = do
	Just . App.Singleton . uncurry (OccGetArea i) <$> atomically ((M.! i) <$> readTVar vm)

main :: IO ()
main = print =<< do
	va <- atomically $ newTVar M.empty
	interpretReact (retry $ handle va) foo

foo :: React s (SetArea :- Singleton GetArea) (Point, Point)
foo = adjust (setArea 0 ((80, 50), (160, 100))) >> adjust (getArea 0)
