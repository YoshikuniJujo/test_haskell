import Prelude
import Control.Arrow((***))

type ZipperL a = ([a], [a])

type Zipper2D a = ZipperL (ZipperL a)

forward, back :: ZipperL a -> ZipperL a

forward z@(_, [_]) = z
forward (bs, f : fs) = (f : bs, fs)

back z@([], _) = z
back (b : bs, fs) = (bs, b : fs)

up, down, left, right :: Zipper2D a -> Zipper2D a

up = back
down = forward
left = map back *** map back
right = map forward *** map forward

sample2d = ([], [
	([], [1, 2, 3]),
	([], [4, 5, 6]),
	([], [7, 8, 9])])
