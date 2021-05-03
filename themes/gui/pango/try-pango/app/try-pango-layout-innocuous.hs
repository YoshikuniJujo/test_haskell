{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Data.Maybe
import Data.CairoImage
import Data.JuicyCairo
import Codec.Picture
import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values
import Graphics.Pango.Values
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage
import Graphics.Pango.Basic.ScriptsAndLanguages.Types
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.TextAttributes
-- import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.Fonts.PangoFontDescription.Type

import qualified Data.Text as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate cairoFormatArgb32 600 1100
	cr <- cairoCreate s

	putStrLn . pangoLanguageGetSampleString =<< pangoLanguageGetDefault
	putStrLn . pangoLanguageGetSampleString $ pangoLanguageFromString "is-is"
	putStrLn . pangoLanguageGetSampleString $ pangoLanguageFromString "zh-tw"
	putStrLn sampleText
	putStrLn sampleText2

	pl <- pangoCairoCreateLayout cr
	print =<< pangoLayoutGet @T.Text pl
	attr0 <- pangoLayoutGet @PangoTextAttrList pl
	print attr0
	{-
	mattr0' <- pangoTextAttrListThaw attr0
	case mattr0' of
		Nothing -> putStrLn "mattr0' is Nothing"
		Just attr0' -> pangoAttrListInsert attr0' =<< pangoAttrNew (Size 20)
		-}
	fd0 <- pangoLayoutGet @PangoFontDescription pl
	print fd0

	print =<< pangoLayoutInfo @CharacterCount pl

	pangoLayoutSet pl $ pangoEllipsizeMiddle
	pangoLayoutSet pl $ Width 180
	pangoLayoutSet pl $ LinesPerParagraph 3
	print =<< pangoLayoutInfo @IsEllipsized pl
	print =<< pangoLayoutInfo @IsWrapped pl

	print =<< pangoLayoutGet @Width pl
	print =<< pangoLayoutGet @Height pl
	pangoLayoutSet pl . T.pack $ sampleText ++ "\n" ++ sampleText2

	print =<< pangoLayoutInfo @IsEllipsized pl
	print =<< pangoLayoutInfo @IsWrapped pl

	print . length $ sampleText ++ "\n" ++ sampleText2
	print =<< pangoLayoutInfo @CharacterCount pl

	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 150
	pangoLayoutSet pl PangoWrapChar
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 300
	pangoLayoutSet pl PangoWrapWordChar
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 450
	pangoLayoutSet pl $ Width 30
	pangoLayoutSet pl PangoWrapWord
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 600
	pangoLayoutSet pl PangoWrapWordChar
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 300 0
	pangoLayoutSet pl $ Width 200
	pangoLayoutSet pl $ Indent 50
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 300 150
	pangoLayoutSet pl . Indent $ - 50
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 300 300
	pangoLayoutSet pl $ Indent 0
	pangoLayoutSet pl $ Spacing 10
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 300 480
	pangoLayoutSet pl $ Width 180
	pangoLayoutSet pl $ Spacing 0
	pangoLayoutSet pl $ Justify True
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 300 630
	pangoLayoutSet pl . T.pack $ "I love sloth. " ++ arabic
	pangoLayoutSet pl $ AutoDir True
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 300 710
	pangoLayoutSet pl $ AutoDir False
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 750
	pangoLayoutSet pl $ Justify False
	pangoLayoutSet pl . T.pack $ sampleText ++ "\n" ++ sampleText2
	pangoLayoutSet pl pangoAlignCenter
	pangoCairoShowLayout cr pl

	cairoMoveTo cr 0 900
	pangoLayoutSet pl pangoAlignRight
	pangoCairoShowLayout cr pl

	pangoLayoutSet pl pangoAlignLeft
	pangoLayoutSet pl pangoEllipsizeNone
	let	txt =  take 60 sampleText' ++ "\n" ++ take 60 sampleText2
	pangoLayoutSet pl $ T.pack txt

	cairoMoveTo cr 300 780
	pangoCairoShowLayout cr pl

	print =<< pangoLayoutInfo @UnknownGlyphsCount pl

	cairoMoveTo cr 300 930
	pangoLayoutSet pl $ SingleParagraphMode True
	pangoCairoShowLayout cr pl

	print =<< pangoLayoutInfo @UnknownGlyphsCount pl
	las <- pangoLayoutInfo pl
	(putStrLn . (\(c, la) -> c ++ "\n" ++ la) . (show *** showPangoLogAttr)) `mapM_` zip (txt ++ "\x00") (pangoLogAttrsToList las)

	print =<< pangoLayoutInfo @Extents pl
	print =<< pangoLayoutInfo @PixelExtents pl
	print =<< pangoLayoutInfo @LayoutSize pl
	print =<< pangoLayoutInfo @LayoutPixelSize pl
	print =<< pangoLayoutInfo @Baseline pl
	print =<< pangoLayoutInfo @LineCount pl

	print =<< pangoLayoutIndexToPos pl 0
	print =<< pangoLayoutIndexToPos pl 1
	print =<< pangoLayoutIndexToPos pl 2
	print =<< pangoLayoutIndexToPos pl 3
	print =<< pangoLayoutIndexToPos pl 4
	print =<< pangoLayoutIndexToPos pl 5
	print =<< pangoLayoutIndexToPos pl 6
	print =<< pangoLayoutIndexToPos pl 7
	print =<< pangoLayoutIndexToPos pl 8
	print =<< pangoLayoutIndexToPos pl 9
	print =<< pangoLayoutIndexToPos pl 10
	print =<< pangoLayoutIndexToPos pl 11
	print =<< pangoLayoutIndexToPos pl 12
	print =<< pangoLayoutIndexToPos pl 13
	print =<< pangoLayoutIndexToPos pl 14

	putStrLn "Oh! Oh!"
	print =<< pangoLayoutIndexToPos pl 120
	print =<< pangoLayoutIndexToPos pl 121
	print =<< pangoLayoutIndexToPos pl 122
	print =<< pangoLayoutIndexToPos pl 123

	print =<< pangoLayoutIndexToLineX pl 0 False
	print =<< pangoLayoutIndexToLineX pl 0 True
	print =<< pangoLayoutIndexToLineX pl 1 False
	print =<< pangoLayoutIndexToLineX pl 1 True
	print =<< pangoLayoutIndexToLineX pl 2 False
	print =<< pangoLayoutIndexToLineX pl 2 True
	print =<< pangoLayoutIndexToLineX pl 60 False
	print =<< pangoLayoutIndexToLineX pl 60 True

	print =<< pangoLayoutXyToIndex pl 10 10
	print =<< pangoLayoutXyToIndex pl 100 10
	print =<< pangoLayoutXyToIndex pl 160 10
	print =<< pangoLayoutXyToIndex pl 180 10
	print =<< pangoLayoutXyToIndex pl 200 10
	print =<< pangoLayoutXyToIndex pl 10 98
	print =<< pangoLayoutXyToIndex pl 20 98
	print =<< pangoLayoutXyToIndex pl 30 98

	print =<< pangoLayoutGetCursorPos pl 0
	print =<< pangoLayoutGetCursorPos pl 1
	print =<< pangoLayoutGetCursorPos pl 108
	print =<< pangoLayoutGetCursorPos pl 109
	print =<< pangoLayoutGetCursorPos pl 110
	print =<< pangoLayoutGetCursorPos pl 111
	print =<< pangoLayoutGetCursorPos pl 112
	print =<< pangoLayoutGetCursorPos pl 113
	print =<< pangoLayoutGetCursorPos pl 114
	print =<< pangoLayoutGetCursorPos pl 115
	print =<< pangoLayoutGetCursorPos pl 116
	print =<< pangoLayoutGetCursorPos pl 117
	print =<< pangoLayoutGetCursorPos pl 118
	print =<< pangoLayoutGetCursorPos pl 119
	print =<< pangoLayoutGetCursorPos pl 120
	print =<< pangoLayoutGetCursorPos pl 121

	print =<< pangoLayoutMoveCursorVisually pl True 0 False L
	print =<< pangoLayoutMoveCursorVisually pl True 0 False R
	print =<< pangoLayoutMoveCursorVisually pl True 0 True R
	print =<< pangoLayoutMoveCursorVisually pl True 1 False R
	print =<< pangoLayoutMoveCursorVisually pl True 17 False R
	print =<< pangoLayoutMoveCursorVisually pl True 18 False R
	print =<< pangoLayoutMoveCursorVisually pl True 19 False R
	print =<< pangoLayoutMoveCursorVisually pl True 120 False R
	print =<< pangoLayoutMoveCursorVisually pl False 120 False R
	print =<< pangoLayoutMoveCursorVisually pl True 121 False R
	print =<< pangoLayoutMoveCursorVisually pl False 121 False R

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-layout-innocuous.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

sampleText, sampleText', sampleText2, arabic :: String
sampleText = unwords $
	pangoLanguageGetSampleString . pangoLanguageFromString <$> [
		"is-is", "ga-ie", "ga", "en", "ja-jp", "zh-tw"
		]

sampleText' = ("\x1f9a5\x1f16f\x1f16e" ++) . unwords $
	pangoLanguageGetSampleString . pangoLanguageFromString <$> [
		"is-is", "ga-ie", "ga", "en", "ja-jp", "zh-tw"
		]

sampleText2 = unwords $
	pangoLanguageGetSampleString . pangoLanguageFromString <$> [
		"af", "ar", "sq"
		]

arabic = pangoLanguageGetSampleString $ pangoLanguageFromString "ar"

pangoLogAttrsToList :: PangoLogAttrs -> [PangoLogAttr]
pangoLogAttrsToList las = fromJust . pangoLogAttrsGetLogAttr las <$> [0 .. pangoLogAttrsGetSize las - 1]

showPangoLogAttr :: PangoLogAttr -> String
showPangoLogAttr la = let PangoLogAttr {
	pangoLogAttrIsLineBreak = lb,
	pangoLogAttrIsMandatoryBreak = mb,
	pangoLogAttrIsCharBreak = cb,
	pangoLogAttrIsWhite = w,
	pangoLogAttrIsCursorPosition = cp,
	pangoLogAttrIsWordStart = ws,
	pangoLogAttrIsWordEnd = we,
	pangoLogAttrIsSentenceBoundary = sb,
	pangoLogAttrIsSentenceStart = ss,
	pangoLogAttrIsSentenceEnd = se,
	pangoLogAttrBackspaceDeleteCharacter = bdc,
	pangoLogAttrIsExpandableSpace = es,
	pangoLogAttrIsWordBoundary = wb
	} = la in
	"is_line_break:              " ++ show lb ++ "\n" ++
	"is_mandatory_break:         " ++ show mb ++ "\n" ++
	"is_char_break:              " ++ show cb ++ "\n" ++
	"is_white:                   " ++ show w ++ "\n" ++
	"is_cursor_posiotn:          " ++ show cp ++ "\n" ++
	"is_word_start:              " ++ show ws ++ "\n" ++
	"is_word_end:                " ++ show we ++ "\n" ++
	"is_sentence_boundary:       " ++ show sb ++ "\n" ++
	"is_sentence_start:          " ++ show ss ++ "\n" ++
	"is_sentence_end:            " ++ show se ++ "\n" ++
	"backspace_delete_character: " ++ show bdc ++ "\n" ++
	"is_expandable_space:        " ++ show es ++ "\n" ++
	"is_word_boundary:           " ++ show wb ++ "\n"
