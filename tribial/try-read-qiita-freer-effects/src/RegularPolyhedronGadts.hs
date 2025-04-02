{-# LANGUAGE GADTs #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RegularPolyhedronGadts where

data Tetrahedron = Tetrahedron Integer deriving Show
data Hexahedron = Hexahedron Integer deriving Show
data Octahedron = Octahedron Integer deriving Show
data Dodecahedron = Dodecahedron Integer deriving Show
data Icosahedron = Icosahedron Integer deriving Show

data FaceArea rh where
	FaceAreaTetra :: Double -> FaceArea Tetrahedron
	FaceAreaHexa :: Integer -> FaceArea Hexahedron
	FaceAreaOcta :: Double -> FaceArea Octahedron
	FaceAreaDodeca :: Double -> FaceArea Dodecahedron
	FaceAreaIcosa :: Double -> FaceArea Icosahedron

class FaceAreable rh where calcFaceArea :: rh -> FaceArea rh

instance FaceAreable Tetrahedron where
	calcFaceArea (Tetrahedron a) =
		FaceAreaTetra $ fromInteger (a * a) * sin (pi / 3) / 2

instance FaceAreable Hexahedron where
	calcFaceArea (Hexahedron a) = FaceAreaHexa $ a * a

instance FaceAreable Octahedron where
	calcFaceArea (Octahedron a) =
		FaceAreaOcta $ fromInteger (a * a) * sin (pi / 3) / 2

instance FaceAreable Dodecahedron where
	calcFaceArea (Dodecahedron a) =
		FaceAreaDodeca $ fromInteger (a * a) * 5 / (4 * tan (pi / 5))

instance FaceAreable Icosahedron where
	calcFaceArea (Icosahedron a) =
		FaceAreaIcosa $ fromInteger (a * a) * sin (pi / 3) / 2


getSurfaceArea :: FaceArea rh -> Double
getSurfaceArea (FaceAreaTetra fa) = 4 * fa
getSurfaceArea (FaceAreaHexa fa) = 6 * fromInteger fa
getSurfaceArea (FaceAreaOcta fa) = 8 * fa
getSurfaceArea (FaceAreaDodeca fa) = 12 * fa
getSurfaceArea (FaceAreaIcosa fa) = 20 * fa

getSurfaceAreaI :: FaceArea rh -> Integer
getSurfaceAreaI (FaceAreaTetra fa) = round $ 4 * fa
getSurfaceAreaI (FaceAreaHexa fa) = 6 * fa
getSurfaceAreaI (FaceAreaOcta fa) = round $ 8 * fa
getSurfaceAreaI (FaceAreaDodeca fa) = round $ 12 * fa
getSurfaceAreaI (FaceAreaIcosa fa) = round $ 20 * fa
