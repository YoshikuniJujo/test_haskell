{-# LANGUAGE TemplateHaskell, QuasiQuotes #-}

module Bitmap where

import File.Binary
import Parse
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Data.List
import Data.Maybe

bitmap :: QuasiQuoter
bitmap = QuasiQuoter {
	quoteExp = mkHaskellTree2 . parseBMP,
	quotePat = undefined,
	quoteType = undefined,
	quoteDec = undefined
 }

mkHaskellTree2 :: BMP -> ExpQ
mkHaskellTree2 bmp = do
	bitmap <- newName "bitmap"
	letE [valD (varP bitmap) (normalB $ recConE 'Bitmap $ mkBody bmp) []] $
		recUpdE (varE bitmap) [
			return ('fileSize, AppE (VarE 'length)
				(SigE (AppE (VarE 'writeBitmap) (VarE bitmap))
					(ConT ''String)))
		 ]
--	 in bitmap { fileSize = length $ (writeBitmap bitmap :: String) }

mkBody :: BMP -> [Q (Name, Exp)]
mkBody bmp = [
	mkLit 'fileSize 0,
	mkLit 'offset $ 54  + 4 * length (colorDic bmp),
	mkLit 'width $ length $ head $ bmpBody bmp,
	mkLit 'height $ length $ bmpBody bmp,
	mkLit 'bitsPerPixel 8,
	mkLit 'compressionMethod 0,
	mkLit 'imageSize $ length $ concat $ bmpBody bmp,
	mkLit 'horizontalResolution 0,
	mkLit 'verticalResolution 0,
	mkLit 'numberOfColors $ length $ colorDic bmp,
	mkLit 'importantColors 0,
	return ('colors, ListE $ map (intLit3 . snd) $ colorDic bmp),
	return ('image, ListE $ map intLit $ convertImage (map fst $ colorDic bmp) $ concat $
				reverse $ bmpBody bmp)
 ]

mkLit :: Name -> Int -> Q (Name, Exp)
mkLit n e = return (n, LitE $ integerL $ fromIntegral e)

intLit :: Int -> Exp
intLit n = LitE $ integerL $ fromIntegral n

intLit3 :: (Int, Int, Int) -> Exp
intLit3 (a, b, c) = TupE [intLit a, intLit b, intLit c]

mkHaskellTree :: BMP -> ExpQ
mkHaskellTree bmp = [|
	let	bmp = testBMP
		bitmap = Bitmap {
			fileSize = 0,
			offset = 54 + 4 * length (colorDic bmp),
			width = length $ head $ bmpBody bmp,
			height = length $ bmpBody bmp,
			bitsPerPixel = 8,
			compressionMethod = 0,
			imageSize = length $ concat $ bmpBody bmp,
			horizontalResolution = 0,
			verticalResolution = 0,
			numberOfColors = length $ colorDic bmp,
			importantColors = 0,
			colors = map snd $ colorDic bmp,
			image = convertImage (map fst $ colorDic bmp) $ concat $
				bmpBody bmp
		 }
	 in bitmap { fileSize = length $ (writeBitmap bitmap :: String) }
 |]

convertImage :: [Char] -> String -> [Int]
convertImage _ "" = []
convertImage is (c : cs) = fromJust (elemIndex c is) : convertImage is cs

[binary|

Bitmap

2: "BM"
4: fileSize
2: 0
2: 0
4: offset
4: 40
4: width
4: height
2: 1
2: bitsPerPixel
4: compressionMethod
4: imageSize
4: horizontalResolution
4: verticalResolution
4: numberOfColors
4: importantColors
4<(Int, Int, Int)>[numberOfColors]: colors
bitsPerPixel/8[imageSize*8/bitsPerPixel]: image

|]
