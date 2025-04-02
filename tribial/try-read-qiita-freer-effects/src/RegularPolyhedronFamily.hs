{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wall -fno-warn-tabs #-}

module RegularPolyhedronFamily where

class SurfaceAreable rh where
	data FaceArea rh
	calcFaceArea :: rh -> FaceArea rh
	getSurfaceArea :: FaceArea rh -> Double

data Tetrahedron = Tetrahedron Integer deriving Show

instance SurfaceAreable Tetrahedron where
	data FaceArea Tetrahedron = FaceAreaTetra Double deriving Show
	calcFaceArea (Tetrahedron a) =
		FaceAreaTetra $ fromInteger (a * a) * sin (pi / 3) / 2
	getSurfaceArea (FaceAreaTetra fa) = 4 * fa

data Hexahedron = Hexahedron Integer deriving Show

instance SurfaceAreable Hexahedron where
	data FaceArea Hexahedron = FaceAreaHexa Integer deriving Show
	calcFaceArea (Hexahedron a) = FaceAreaHexa $ a * a
	getSurfaceArea (FaceAreaHexa fa) = 6 * fromInteger fa
