{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.State
import Data.Maybe
import Data.Set
import Data.Time

import React
import ColoredBoxes
import GuiEv

import Field
import ButtonEvent

main :: IO ()
main = do
	f <- openField "スリープ" [exposureMask, buttonPressMask]
	t <- getCurrentTime
	(interpret (handle f) doubler `runStateT` t) >>= print
	closeField f

handle :: Field -> EvReqs GuiEv -> StateT UTCTime IO (EvOccs GuiEv)
handle f r = do
	t <- get
	n <- liftIO getCurrentTime
	put n
	if n `diffUTCTime` t >= time
	then do	put $ addUTCTime time t
		eventIf r (makeTimeObs r time) (handle f)
	else withNextEventTimeout f 50000 \case
		[] -> eventIf r (makeTimeObs r $ n `diffUTCTime` t) (handle f)
		es -> case eventsToGuiEv es of
			Just ges -> eventIf r ges (handle f)
			Nothing -> handle f r
--		es -> liftIO (putStrLn $ "event occur: " ++ show es) >> handle f r
	where time = getWait r

eventIf :: (Monad m, Ord e) => EvReqs e -> EvOccs e -> (EvReqs e -> m (EvOccs e)) -> m (EvOccs e)
eventIf r evs hdl
	| evs' == empty = hdl r
	| otherwise = pure evs'
	where
	evs' = evs `intersection` r

getWait :: Set GuiEv -> Time
getWait r = if Prelude.null al then big else minimum al where
	al = filterMap getTime (elems r)
	getTime (TryWait t _) = Just t
	getTime _ = Nothing

filterMap :: (a -> Maybe b) -> [a] -> [b]
filterMap = (catMaybes .) . Prelude.map

big :: Time
big = 2 ^ (2 ^ (5 :: Int) :: Int)

makeTimeObs :: Set GuiEv -> Time -> Set GuiEv
makeTimeObs r t = fromList . filterMap makeOcc $ elems r where
	makeOcc (TryWait t' _) = Just $ TryWait t' (Occurred t)
	makeOcc (DeltaTime _) = Just $ DeltaTime (Occurred t)
	makeOcc _ = Nothing

eventsToGuiEv :: [Field.Event] -> Maybe (EvOccs GuiEv)
eventsToGuiEv evs = case mapMaybe ((buttonEventToGuiEv =<<) . buttonEvent) evs of
	[] -> Nothing
	ge -> Just $ fromList ge

buttonEventToGuiEv :: ButtonEvent -> Maybe GuiEv
buttonEventToGuiEv BtnEvent {
	buttonNumber = bn,
	pressOrRelease = Press,
	position = (_x, _y) } = do
	mb <- buttonNumberToMouseBtn bn
	pure . MouseDown $ Occurred [mb]
buttonEventToGuiEv _ = Nothing

buttonNumberToMouseBtn :: ButtonNumber -> Maybe MouseBtn
buttonNumberToMouseBtn Button1 = Just MLeft
buttonNumberToMouseBtn Button2 = Just MMiddle
buttonNumberToMouseBtn Button3 = Just MRight
buttonNumberToMouseBtn _ = Nothing
