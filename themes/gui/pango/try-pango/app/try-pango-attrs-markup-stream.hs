{-# LANGUAGE BlockArguments, LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Monad.ST
import Data.CairoImage
import Data.JuicyCairo
import System.Environment
import System.Glib.ErrorReporting
import Codec.Picture
import Graphics.Cairo.Values
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Pango.Basic.TextAttributes
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Rendering.Cairo

import System.Glib.SimpleXmlSubsetParser

import qualified Data.Text as T

sample :: T.Text
sample = "<big><i>Hello</i>, <b>world</b>!</big>"

pangoParseMarkup' ::
	[T.Text] -> Maybe Char -> Either GError (PangoTextAttrList, Maybe Char)
pangoParseMarkup' ts mc = runST do
	pc <- pangoMarkupParserNew mc
	r <- gMarkupParseContextParse pc `eitherMapM_` ts
	case r of
		Left e -> pure $ Left e
		Right () -> pangoMarkupParserFinish pc

eitherMapM :: Monad m => (a -> m (Either b c)) -> [a] -> m (Either b [c])
eitherMapM f = \case
	[] -> pure $ Right []
	x : xs -> f x >>= \case
		Left e -> pure $ Left e
		Right r -> ((r :) <$>) <$> eitherMapM f xs

eitherMapM_ :: Monad m => (a -> m (Either b c)) -> [a] -> m (Either b ())
eitherMapM_ f = \case
	[] -> pure $ Right ()
	x : xs -> f x >>= \case
		Left e -> pure $ Left e;
		Right _ -> eitherMapM_ f xs

main :: IO ()
main = do
	mu <- (<$> getArgs) \case
		[] -> [sample]
		args -> T.pack <$> args
	case pangoParseMarkup' mu Nothing of
		Left e -> do
			putStrLn $ gErrorReport e
			case e of
				GErrorMarkup c m -> do
					print c
					putStrLn case c of
						GMarkupErrorBadUtf8 -> "GMarkupErrorBadUtf8"
						GMarkupErrorEmpty -> "GMarkupErrorEmpty"
						GMarkupErrorParse -> "GMarkupErrorParse"
						GMarkupErrorUnknownElement -> "GMarkupErrorUnknownElement"
						GMarkupErrorUnknownAttribute -> "GMarkupErrorUnknownAttribute"
						GMarkupErrorInvalidContent -> "GMarkupErrorInvalidContent"
						GMarkupErrorMissingAttribute -> "GMarkupErrorMissingAttribute"
						_ -> show c
					putStrLn m
				_ -> putStrLn "No GMarkupError"
		Right (tpal, _) -> do
			s <- cairoImageSurfaceCreate cairoFormatArgb32 300 400
			cr <- cairoCreate s

			pl <- pangoCairoCreateLayout cr
			pangoLayoutSet pl tpal
			pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

			cairoImageSurfaceGetCairoImage s >>= \case
				CairoImageArgb32 a -> writePng "try-pango-attrs-markup-stream.png" $ cairoArgb32ToJuicyRGBA8 a
				_ -> error "never occur"
