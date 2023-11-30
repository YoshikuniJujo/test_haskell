{-# LANGUAGE LambdaCase, OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module Main where

import Control.Arrow
import Data.Maybe
import Data.CairoImage.Internal
import Data.JuicyCairo
import Codec.Picture

import Graphics.Cairo.Drawing.CairoT
import Graphics.Cairo.Drawing.Paths
import Graphics.Cairo.Surfaces.ImageSurfaces
import Graphics.Cairo.Values

import Graphics.Pango.Basic.GlyphStorage
import Graphics.Pango.Basic.Fonts.PangoFontDescription
import Graphics.Pango.Basic.LayoutObjects.PangoLayout
import Graphics.Pango.Basic.ScriptsAndLanguages.PangoLanguage
import Graphics.Pango.Rendering.Cairo

import Graphics.Pango.Basic.TextAttributes

import qualified Data.Text as T
import qualified Data.Text.IO as T

main :: IO ()
main = do
	s <- cairoImageSurfaceCreate CairoFormatArgb32 600 1100
	cr <- cairoCreate s

	T.putStrLn . pangoLanguageGetSampleString =<< pangoLanguageGetDefault
	T.putStrLn . pangoLanguageGetSampleString $ PangoLanguage "is-is"
	T.putStrLn . pangoLanguageGetSampleString $ PangoLanguage "zh-tw"
	T.putStrLn sampleText
	T.putStrLn sampleText2

	pl <- pangoCairoCreateLayout cr
	print . pangoLayoutGet @T.Text =<< pangoLayoutFreeze pl
	attr0 <- pangoLayoutGet @PangoTextAttrList <$> pangoLayoutFreeze pl
	print attr0
	{-
	mattr0' <- pangoTextAttrListThaw attr0
	case mattr0' of
		Nothing -> putStrLn "mattr0' is Nothing"
		Just attr0' -> pangoAttrListInsert attr0' =<< pangoAttrNew (Size 20)
		-}
	fd0 <- pangoLayoutGet @PangoFontDescriptionNullable <$> pangoLayoutFreeze pl
	print fd0

	print . pangoLayoutInfo @CharacterCount =<< pangoLayoutFreeze pl

	pangoLayoutSet pl $ PangoEllipsizeMiddle
	pangoLayoutSet pl $ Width 180
	pangoLayoutSet pl $ LinesPerParagraph 3
	print . pangoLayoutInfo @IsEllipsized =<< pangoLayoutFreeze pl
	print . pangoLayoutInfo @IsWrapped =<< pangoLayoutFreeze pl

	print . pangoLayoutGet @Width =<< pangoLayoutFreeze pl
	print . pangoLayoutGet @Height =<< pangoLayoutFreeze pl
	pangoLayoutSet pl $ sampleText <> "\n" <> sampleText2

	print . pangoLayoutInfo @IsEllipsized =<< pangoLayoutFreeze pl
	print . pangoLayoutInfo @IsWrapped =<< pangoLayoutFreeze pl

	print . T.length $ sampleText <> "\n" <> sampleText2
	print . pangoLayoutInfo @CharacterCount =<< pangoLayoutFreeze pl

	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 150
	pangoLayoutSet pl PangoWrapChar
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 300
	pangoLayoutSet pl PangoWrapWordChar
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 450
	pangoLayoutSet pl $ Width 30
	pangoLayoutSet pl PangoWrapWord
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 600
	pangoLayoutSet pl PangoWrapWordChar
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 300 0
	pangoLayoutSet pl $ Width 200
	pangoLayoutSet pl $ Indent 50
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 300 150
	pangoLayoutSet pl . Indent $ - 50
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 300 300
	pangoLayoutSet pl $ Indent 0
	pangoLayoutSet pl $ Spacing 10
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 300 480
	pangoLayoutSet pl $ Width 180
	pangoLayoutSet pl $ Spacing 0
	pangoLayoutSet pl $ Justify True
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 300 630
	pangoLayoutSet pl $ "I love sloth. " <> arabic
	pangoLayoutSet pl $ AutoDir True
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 300 710
	pangoLayoutSet pl $ AutoDir False
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 750
	pangoLayoutSet pl $ Justify False
	pangoLayoutSet pl $ sampleText <> "\n" <> sampleText2
	pangoLayoutSet pl PangoAlignCenter
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	cairoMoveTo cr 0 900
	pangoLayoutSet pl PangoAlignRight
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	pangoLayoutSet pl PangoAlignLeft
	pangoLayoutSet pl PangoEllipsizeNone
	let	txt =  T.take 60 sampleText' <> "\n" <> T.take 60 sampleText2
	pangoLayoutSet pl txt

	cairoMoveTo cr 300 780
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	print . pangoLayoutInfo @UnknownGlyphsCount =<< pangoLayoutFreeze pl

	cairoMoveTo cr 300 930
	pangoLayoutSet pl $ SingleParagraphMode True
	pangoCairoShowLayout cr =<< pangoLayoutFreeze pl

	print . pangoLayoutInfo @UnknownGlyphsCount =<< pangoLayoutFreeze pl
	las <- pangoLayoutInfo <$> pangoLayoutFreeze pl
	(putStrLn . (\(c, la) -> c ++ "\n" ++ la) . (show *** showPangoLogAttr)) `mapM_` zip (T.unpack $ txt <> "\x00") (pangoLogAttrsToList las)

	print . pangoLayoutInfo @Extents =<< pangoLayoutFreeze pl
	print . pangoLayoutInfo @PixelExtents =<< pangoLayoutFreeze pl
	print . pangoLayoutInfo @LayoutSize =<< pangoLayoutFreeze pl
	print . pangoLayoutInfo @LayoutPixelSize =<< pangoLayoutFreeze pl
	print . pangoLayoutInfo @Baseline =<< pangoLayoutFreeze pl
	print . pangoLayoutInfo @LineCount =<< pangoLayoutFreeze pl

{-
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
	-}

	cairoImageSurfaceGetCairoImage s >>= \case
		CairoImageArgb32 a -> writePng "try-pango-layout-innocuous.png" $ cairoArgb32ToJuicyRGBA8 a
		_ -> error "never occur"

sampleText, sampleText', sampleText2, arabic :: T.Text
sampleText = T.unwords $
	pangoLanguageGetSampleString . PangoLanguage <$> [
		"is-is", "ga-ie", "ga", "en", "ja-jp", "zh-tw"
		]

sampleText' = ("\x1f9a5\x1f16f\x1f16e" <>) . T.unwords $
	pangoLanguageGetSampleString . PangoLanguage <$> [
		"is-is", "ga-ie", "ga", "en", "ja-jp", "zh-tw"
		]

sampleText2 = T.unwords $
	pangoLanguageGetSampleString . PangoLanguage <$> [
		"af", "ar", "sq"
		]

arabic = pangoLanguageGetSampleString $ PangoLanguage "ar"

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
