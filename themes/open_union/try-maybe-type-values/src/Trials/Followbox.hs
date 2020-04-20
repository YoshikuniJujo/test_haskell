{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeOperators #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Trials.Followbox where

import Prelude hiding (until, repeat)

import Control.Monad
import Data.Type.Flip
import Data.Type.Set
import Data.Or
import Data.String
import Data.Time.Clock.POSIX
import Data.Time.LocalTime hiding (getTimeZone)
import System.Random hiding (next)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import qualified Data.HashMap.Strict as HM

import qualified Data.ByteString.Lazy as LBS
import qualified Codec.Picture as JP
import qualified Codec.Picture.Extra as JP

import Trials.Followbox.Event
import Trials.Followbox.View
import Trials.Followbox.Wrapper.Aeson
import Trials.Followbox.Wrapper.XGlyphInfo
import MonadicFrp

-- type Uri = String

apiUsers :: Int -> Uri
apiUsers s = "https://api.github.com/users?since=" <> show s

getUsersJson :: React (
	StoreRandomGen :- LoadRandomGen :- HttpGet :- BeginSleep :- EndSleep :- 'Nil
	) (Either String [Object])
getUsersJson = do
	g <- adjust loadRandomGen
	let	(n, g') = randomR (0, 2 ^ (27 :: Int)) g
	adjust $ storeRandomGen g'
	(h, b) <- adjust . httpGet $ apiUsers n
	let	mrmn = read . BSC.unpack <$> lookup (fromString "X-RateLimit-Remaining") h :: Maybe Int
		mrst = posixSecondsToUTCTime . fromInteger . read . BSC.unpack <$> lookup (fromString "X-RateLimit-Reset") h
	case (mrmn, mrst) of
		(Just rmn, _) | rmn > 0 -> pure $ decodeJson b
		(Just _, Just t) -> adjust (beginSleep t) >> adjust endSleep >> getUsersJson
		(Just _, Nothing) -> error "No X-RateLimit-Reset"
		(Nothing, _) -> error "No X-RateLimit-Remaining header"

viewResetTimeReact :: React (BeginSleep :- GetTimeZone :- 'Nil) View
viewResetTimeReact = do
	t <- adjust checkBeginSleep
	tz <- adjust getTimeZone
	pure [Text white 30 (100, 500) $ "Wait until " <> T.pack (show $ utcToLocalTime tz t)]

viewResetTime :: SigF View ()
viewResetTime = do
	emit []
	emit =<< waitFor (adjust viewResetTimeReact)
	waitFor $ adjust endSleep
	viewResetTime

getUser1 :: ReactF Object
getUser1 = adjust loadJsons >>= \case
	[] -> adjust getUsersJson >>= \case
		Right (o : os) -> o <$ adjust (storeJsons $ take 8 os)
		Right [] -> adjust (raiseError EmptyJson "Empty JSON") >> getUser1
		Left em -> adjust (raiseError NotJson em) >> getUser1
	o : os -> o <$ adjust (storeJsons os)

getUserN :: Int -> ReactF [Object]
getUserN n = n `replicateM` getUser1

getLoginName :: Object -> ReactF T.Text
getLoginName o = pure (loginNameFromObject o) >>= \case
	Just ln -> pure ln
	Nothing -> adjust (raiseError NoLoginName "No Login Name") >> getLoginName o

getAvatarAddress :: Object -> ReactF T.Text
getAvatarAddress o = avatorAddressFromObject <$> getUser1 >>= \case
	Just ln -> pure ln
	Nothing -> adjust (raiseError NoAvatarAddress "No Avatar Address") >> getAvatarAddress o

getHtmlUrl :: Object -> ReactF T.Text
getHtmlUrl o = pure (htmlUrlFromObject o) >>= \case
	Just u -> pure u
	Nothing -> adjust (raiseError NoHtmlUrl "No HTML URL") >> getHtmlUrl o

viewAvatar :: SigF View ()
viewAvatar = do
	v <- waitFor viewAvatarReact
	emit v
	waitFor $ adjust leftClick

type Avatar = JP.Image JP.PixelRGBA8

viewAvatarReact :: ReactF View
viewAvatarReact = (: []) . Image (100, 100) <$> (viewAvatarGen =<< getUser1)

viewAvatarGen :: Object -> ReactF Avatar
viewAvatarGen o = do
	url <- getAvatarAddress o
	ea <- (scale 80 80 <$>) . bsToImage . snd <$> adjust (httpGet $ T.unpack url)
	case ea of
		Left em -> adjust (raiseError NoAvatar em) >> viewAvatarGen o
		Right v -> pure v

leftClickUserN :: Int -> ReactF [Maybe T.Text]
leftClickUserN n = adjust leftClick >> (loginNameFromObject <$>) <$> getUserN n

loginNameFromObject, avatorAddressFromObject, htmlUrlFromObject :: Object -> Maybe T.Text
loginNameFromObject o = case HM.lookup "login" o of
	Just (String li) -> Just li; _ -> Nothing

avatorAddressFromObject o = case HM.lookup "avatar_url" o of
	Just (String li) -> Just li; _ -> Nothing

htmlUrlFromObject o = case HM.lookup "html_url" o of
	Just (String li) -> Just li; _ -> Nothing

terminateOccur :: ReactF ()
terminateOccur = adjust catchError >>= \case
	Continue -> terminateOccur
	Terminate -> pure ()

getUser1UntilError :: ReactF (Or Object ())
getUser1UntilError = getUser1 `first` terminateOccur

getLoginNameQuit :: SigF View (Either (T.Text, ()) (Maybe ()))
getLoginNameQuit = loginNameToView
	<$%> repeat (adjust leftClick >> (getLoginName =<< getUser1)) `until` checkQuit

loginNameToView :: T.Text -> View
loginNameToView nm = [Text blue 24 (100, 100) nm]

loginNameNToView :: Int -> T.Text -> View1
loginNameNToView n nm = Text blue 24 (100, 100 + fromIntegral n * 50) nm

getLoginNameNQuit :: SigF View (Either (View, ()) (Maybe ()))
getLoginNameNQuit = (repeat (adjust leftClick >> arrangeLoginNameN 3) `until` checkQuit)

arrangeLoginNameN :: Integer -> ReactF View
arrangeLoginNameN n = (concat . (fst3 <$>)) <$> viewLoginName `mapM` [0 .. n - 1]

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

createLoginName :: Color -> Double -> Position -> T.Text -> Position -> Rect -> (View, Rect, Rect)
createLoginName clr fs p t (x', y') r' = (
	Text clr fs p t : createX 4 (round fs `div` 2) (x' + round fs `div` 2, y' + round fs * 3 `div` 8),
	Rect	(x' + round fs `div` 2, y' + round fs * 3 `div` 8)
		(x' + round fs, y' + round fs * 7 `div` 8),
	r' )

createX :: Integer -> Integer -> Position -> View
createX lw sz (x, y) = [
	Line white lw (x, y) (x + sz, y + sz),
	Line white lw (x + sz, y) (x, y + sz) ]

data Rect = Rect { upperLeft :: Position, bottomRight :: Position }
	deriving Show

getAvatarLoginName :: ReactF (Avatar, T.Text, T.Text)
getAvatarLoginName = do
	obj <- getUser1
	(,,) <$> viewAvatarGen obj <*> getLoginName obj <*> getHtmlUrl obj

viewMultiLoginName :: Integer -> SigF View ()
viewMultiLoginName n = (<>) <$%> viewMultiLoginNameSig n <*%> viewResetTime

viewMultiLoginNameSig :: Integer -> SigF View ()
viewMultiLoginNameSig n = do
	(vn, rn) <- waitFor next
	(vr, rr) <- waitFor refresh
	emit $ title : vn ++ vr
	vmlns (vn, rn) (vr, rr)
	where
	vmlns nxt@(vn, rn) rfs@(vr, rr) = do
		lns <- fromIntegral n `replicateM` waitFor getAvatarLoginName
		r <- ((title :) . (vn ++) . (vr ++) . concat <$%> ftraverse (uncurry viewLoginNameSig) ([0 .. n - 1] `zip` lns))
			`until` (clickOnRect rn `first` clickOnRect rr)
		case r of
			Left (_, L _) -> pure ()
			Left (_, LR _ _) -> pure ()
			Left (_, R _) -> do
				emit $ title : vn ++ vr
				waitFor . adjust $ storeJsons []
			Right _ -> error "never occur"
		vmlns nxt rfs

viewLoginNameSig :: Integer -> (Avatar, T.Text, T.Text) -> SigF View ()
viewLoginNameSig n (a, ln, u) = do
	(v, r, r') <- waitFor $ createLoginName1 n ln
	emit $ Image (100, 120 + 120 * n) a : v
	void $ waitFor (forever $ clickOnRect r' >> adjust (browse $ T.unpack u)) `until` clickOnRect r
	viewLoginNameSig n =<< waitFor getAvatarLoginName

viewLoginName :: Integer -> ReactF (View, Rect, Rect)
viewLoginName n = createLoginName1 n =<< getLoginName =<< getUser1

createLoginName1 :: Integer -> T.Text -> ReactF (View, Rect, Rect)
createLoginName1 n t = do
	XGlyphInfo {
		xGlyphInfoWidth = w,
		xGlyphInfoHeight = h,
		xGlyphInfoX = x,
		xGlyphInfoY = y } <- adjust $ calcTextExtents "sans" 36 t
	pure $ createLoginName blue 36 (210, 180 + n * 120) t (210 - x + w, 180 + n * 120 - y)
		(Rect (210 - x, 180 + n * 120 - y) (210 - x + w, 180 + n * 120 - y + h))

clickOnRect :: Rect -> ReactF ()
clickOnRect r = () <$ find (`isInsideOf` r) (mousePosition `indexBy` repeat leftClick)

mousePosition :: SigF Position ()
mousePosition = repeat $ adjust move

isInsideOf :: Position -> Rect -> Bool
isInsideOf (x, y) (Rect (l, t) (r, b)) = l <= x && x <= r && t <= y && y <= b

title :: View1
title = Text white 36 (50, 80) "Who to follow"

linkText :: Double -> Position -> T.Text -> ReactF (View, Rect)
linkText fs p@(x0, y0) t = do
	XGlyphInfo {
		xGlyphInfoWidth = w,
		xGlyphInfoHeight = h,
		xGlyphInfoX = x,
		xGlyphInfoY = y } <- adjust $ calcTextExtents "sans" fs t
	pure (	[	Text blue fs p t,
			Line blue 4
				(x0 - x, y0 + 6)
				(x0 + w - x, y0 + 6) ],
		Rect	(x0 - x, y0 - y)
			(x0 - x + w, y0 - y + h) )

next, refresh :: ReactF (View, Rect)
next = linkText 30 (500, 80) "Next"
refresh = linkText 30 (600, 80) "Refresh"

bsToImage :: LBS.ByteString -> Either String (JP.Image JP.PixelRGBA8)
bsToImage lbs = JP.convertRGBA8 <$> JP.decodeImage (LBS.toStrict lbs)

scale :: Integer -> Integer -> JP.Image JP.PixelRGBA8 -> JP.Image JP.PixelRGBA8
scale w_ h_ = JP.scaleBilinear w h where [w, h] = fromIntegral <$> [w_, h_]
