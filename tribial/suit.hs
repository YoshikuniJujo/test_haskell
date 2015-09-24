
data Suit = Spade | Heart | Diamond | Club

data Color = Black | Red

color :: Suit -> Color
color Spade = Black
color Heart = Red
color Diamond = Red
color Club = Black

rgb :: Color -> (Int, Int, Int)
rgb Black = (0, 0, 0)
rgb Red = (0xff, 0, 0)
