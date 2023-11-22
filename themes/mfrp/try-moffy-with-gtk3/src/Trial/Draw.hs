{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE TypeApplications, ViewPatterns #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trial.Draw where

import Prelude hiding (repeat, break, until, filter, scanl)

import qualified Control.Arrow as A
import Control.Monad
import Control.Moffy
import Control.Moffy.Event.Window
import Control.Moffy.Event.DefaultWindow
import Control.Moffy.Event.ThreadId
import Control.Moffy.Event.Lock
import Control.Moffy.Event.Mouse.DefaultWindow
import Control.Moffy.Viewable.Basic
import Control.Moffy.Viewable.Shape
import Data.Type.Set
import Data.Maybe
import Data.Word

import Control.Moffy.Handle
import Control.Moffy.Handle.ThreadId
import Control.Moffy.Handle.Lock
import Control.Moffy.Handle.TChan
import Control.Moffy.Handle.DefaultWindow
import Control.Moffy.Run.TChan
import Control.Moffy.Run.GtkField

import Graphics.Gtk

import Data.Type.Flip
import Data.OneOfThem as Oot
import Data.Or

import qualified Data.Set as D
import qualified Data.List as L

import Trial.Draw.OneOfThem
import Trial.Draw.Event
import Trial.Draw.Handle
import Trial.Draw.Viewable
import qualified Trial.Draw.Marshal as M

import Debug.Trace

import qualified Data.Map as Map

type Viewable = OneOfThem (Box :- Line :- FillPolygon :- Message :- 'Nil)
type Events = LoadDefaultWindow :- WindowNew :- GetThreadId :- MouseEv :+: LockEv :+: LinesEv

shapeToViewable :: M.Shape -> Viewable
shapeToViewable (M.Line s e) = Oot.expand . Singleton $ Line' (Color 0 0 0) 2 s e
shapeToViewable (M.FillPolygon c ps) = Oot.expand . Singleton $ FillPolygon (mcolorToColor c) ps

viewableToShape :: Viewable -> Maybe M.Shape
viewableToShape v = case (project v, project v) of
	(Just (Line' (Color 0 0 0) 2 s e), _) -> Just $ M.Line s e
	(_, Just (FillPolygon c ps)) -> Just $ M.FillPolygon (colorToMcolor c) ps
	_ -> Nothing

mcolorToColor :: M.Color -> Color
mcolorToColor (M.Color r g b) = Color (round $ r * 0xff) (round $ g * 0xff) (round $ b * 0xff)

colorToMcolor :: Color -> M.Color
colorToMcolor (Color r g b) = M.Color (fromIntegral r / 0xff) (fromIntegral g / 0xff) (fromIntegral b / 0xff)

first' :: Firstable es es' a a => React s es a -> React s es' a -> React s (es :+: es') a
first' l r = first l r >>= \case
	L x -> pure x
	R y -> pure y
	LR x _ -> pure x

maybeEither :: b -> Either a (Maybe b, ()) -> b
maybeEither d (Left _) = d
maybeEither d (Right (Nothing, ())) = d
maybeEither _ (Right (Just x, ())) = x

maybeEither2 :: r' -> Either r (Maybe a, r') -> r'
maybeEither2 d (Left _) = d
maybeEither2 _ (Right (_, x)) = x

rectR, rectG, rectB :: Rect
rectR = Rect (25, 25) (75, 75)
rectG = Rect (100, 25) (150, 75)
rectB = Rect (175, 25) (225, 75)

rectangleAndLines :: [M.Shape] -> Sig s Events [Viewable] ()
rectangleAndLines ((shapeToViewable <$>) -> v0) = do
	li0 <- waitFor $ adjust newLockId
	li <- waitFor $ adjust newLockId
	((v0 ++) . sortType @('[FillPolygon, Box, Line, Message]) <$%>) $ (\yr ls bx gp msg -> yr ++ ls ++ bx ++ gp ++ msg)
		<$%> ((>> waitFor never)
			$ emit [
				Oot.expand . Singleton $ Box rectR Red,
				Oot.expand . Singleton $ Box rectG Green,
				Oot.expand . Singleton $ Box rectB Blue
				])
		<*%> (emit [] >> sampleLine li0 li)
		<*%> do	emit []
			waitFor (adjust clickOnBox)
			emit [	Oot.expand . Singleton $ Message "Yellow Box have clicked",
				Oot.expand . Singleton $ Box (Rect (200, 200) (250, 250)) Red]
			waitFor never
		<*%> morePolygon
		<*%> ((: []) . Oot.expand . Singleton . Message . ("here " ++) . show <$%> adjustSig colorScroll)

colorScroll :: Sig s (LoadDefaultWindow :- MouseEv) Color ()
colorScroll = Color <$%> rectY rectR <*%> rectY rectG <*%> rectY rectB

rectY :: Rect -> Sig s (LoadDefaultWindow :- MouseEv) Word8 ()
rectY r = scanl add 0x7f . (snd <$%>) . filter ((`insideRect` r) . fst) $ repeat posAndDeltaY

add :: Word8 -> Int -> Word8
add n d
	| 0 <= nd && nd <= 255 = fromIntegral nd
	| otherwise = n
	where
	nd = fromIntegral n + d

posAndDeltaY :: React s (LoadDefaultWindow :- MouseEv) (Point, Int)
posAndDeltaY = adjust $ A.first fromJust . fromR <$> mousePos `at` (negate . round . snd <$> mouseScroll)

filter :: (a -> Bool) -> Sig s es a r -> Sig s es a r
filter p s = waitFor (find p s) >>= either ((>> filter p s) . emit) pure

fromR :: Either l r -> r
fromR (Right r) = r
fromR _ = error "bad"

adjustPoint1 :: Point -> Point -> Maybe Point
adjustPoint1 (x0, y0) (x, y)
	| x0 - 10 <= x && x <= x0 + 10 &&
		y0 - 10 <= y && y <= y0 + 10 = Just (x0, y0)
	| otherwise = Nothing

adjustPoint :: [Point] -> Point -> Maybe Point
adjustPoint ps0 p = listToMaybe $ mapMaybe (`adjustPoint1` p) ps0

grayPolygon :: Sig s Events [Viewable] ()
grayPolygon = (((: []) . Oot.expand . Singleton) .) . FillPolygon <$%> adjustSig colorScroll <*%> polygonPoints []

stopPolygon :: Sig s Events [Viewable] [Viewable]
stopPolygon = fst . fromR <$> grayPolygon `until` leftClick

morePolygon :: Sig s Events [Viewable] ()
morePolygon = do
	p <- stopPolygon
	(p ++) <$%> morePolygon

polygonPoints :: [Point] -> Sig s Events [Point] ()
polygonPoints ps = do
	emit ps
	cp <- waitFor . adjust $ maybeEither (0, 0) <$> mousePos `at` rightClick
	ls <- waitFor $ adjust loadLines
	let	ps0 = linesToPoints ls
	polygonPoints . maybe id (:) (adjustPoint ps0 cp) $ ps

clickOnBox :: React s (LoadDefaultWindow :- MouseEv) ()
clickOnBox = void . adjust $ find (`insideRect` Rect (50, 50) (100, 100)) (fst <$%> mousePos `indexBy` repeat leftClick)

clickOnRect, upOnRect, rightOnRect :: Rect -> React s (LoadDefaultWindow :- MouseEv) ()
clickOnRect r = void . adjust $ find (`insideRect` r) (fst <$%> mousePos `indexBy` repeat leftClick)
upOnRect r = void . adjust $ find (`insideRect` r) (fst <$%> mousePos `indexBy` repeat leftUp)
rightOnRect r = void . adjust $ find (`insideRect` r) (fst <$%> mousePos `indexBy` repeat rightClick)

insideRect :: Point -> Rect -> Bool
insideRect (x, y) (Rect (l, t) (r, b)) = l <= x && x <= r && t <= y && y <= b

sampleLine :: LockId -> LockId -> Sig s Events [Viewable] ()
sampleLine li0 li = do
	_ <- (concat <$%>) .  parList $ spawn do
		(s, e) <- withLockSig li0 do
			s <- waitFor clickPoint
			e <- maybeEither2 (0, 0) <$> (makeLineAndPoint s <$%> mousePos `break` upPoint)
			waitFor . adjust $ addLine li (s, e)
			pure (s, e)
		(\l ss es -> l ++ ss ++ es)
			<$%> (emit (makeLine s e) >> waitFor never)
			<*%> do	emit $ makePoint Yellow s
				void . waitFor $ pointToReactRight s
				emit $ makePoint Red s
				waitFor never'
			<*%> do	emit $ makePoint Yellow e
				void . waitFor $ pointToReactRight e
				emit $ makePoint Red e
				waitFor never'
	waitFor never

never' :: React s es ()
never' = never

clickPoint :: React s Events Position
clickPoint = do
	ls <- adjust loadLines
	let	r = trace ("clickPoint: " ++ show (linesToPoints ls) ++ "\n") $ linesToReact ls
	r `first'` (maybeEither (0, 0) <$> mousePos `at` leftClick)

upPoint :: React s Events Position
upPoint = do
	ls <- adjust loadLines
	let	r = trace ("upPoint: " ++ show (linesToPoints ls) ++ "\n") $ linesToReactUp ls
	r `first'` (maybeEither (0, 0) <$> mousePos `at` leftUp)

makeLineAndPoint, makeLine :: Point -> Point -> [Viewable]
makeLineAndPoint s e = makePoint Yellow s ++ makePoint Yellow e ++ makeLine s e
makeLine s e = [Oot.expand . Singleton $ Line' (Color 0 0 0) 2 s e]

makePoint :: BColor -> Point -> [Viewable]
makePoint c (x, y) = [
	Oot.expand . Singleton $ Box (Rect (x - 5, y - 5) (x + 5, y + 5)) c ]

data DrawState = DrawState {
	dsNextLockId :: Int,
	dsLocks :: [LockId],
	dsLines :: D.Set SimpleLine,
	dsDefaultWindow :: Maybe WindowId }
	deriving Show

instance LockState DrawState where
	getNextLockId = dsNextLockId
	putNextLockId s li = s { dsNextLockId = li }
	isLocked s li = li `elem` dsLocks s
	lockIt s li = s { dsLocks = li : dsLocks s }
	unlockIt s li = s { dsLocks = L.delete li (dsLocks s) }

instance LinesState DrawState where
	getLines = dsLines
	putLines s ls = s { dsLines = ls }

instance DefaultWindowState DrawState where
	getDefaultWindow = dsDefaultWindow
	putDefaultWindow s dw = s { dsDefaultWindow = Just dw }

runDraw :: (Monoid a, Adjustable es (StoreDefaultWindow :- Events :+: GuiEv)) => GtkDrawer' a -> Sig s es (Map.Map WindowId a) r -> IO (r, DrawState)
runDraw dr s = do
	([], (cr, c, c')) <- runGtkMain dr []
	interpretSt (retrySt $
		(handleDefaultWindow `mergeSt`
		liftHandle' handleGetThreadId `mergeSt`
		handleLock `mergeSt`
		handleLines) `beforeSt`
		liftHandle' (handle Nothing cr c)
		) c' s (DrawState 0 [] D.empty Nothing) <* gtkMainQuit

linesToPoints :: D.Set SimpleLine -> [Position]
linesToPoints = concatMap (\(x, y) -> [x, y]) . D.toList

pointToReact, pointToReactUp, pointToReactRight :: Position -> React s Events Position
pointToReact (x, y) = do
	adjust . clickOnRect $ Rect (x - 10, y - 10) (x + 10, y + 10)
	pure (x, y)

pointToReactUp (x, y) = do
	adjust . upOnRect $ Rect (x - 10, y - 10) (x + 10, y + 10)
	pure (x, y)

pointToReactRight (x, y) = do
	adjust . rightOnRect $ Rect (x - 10, y - 10) (x + 10, y + 10)
	pure (x, y)

linesToReact, linesToReactUp, linesToReactRight :: D.Set SimpleLine -> React s Events Position
linesToReact = foldr first' never . map pointToReact . linesToPoints
linesToReactUp = foldr first' never . map pointToReactUp . linesToPoints
linesToReactRight = foldr first' never . map pointToReactRight . linesToPoints
