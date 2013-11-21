import Prelude hiding (showParen, showString)

showParen :: Bool -> ShowS -> ShowS
showParen True f = ("(" ++) . f . (")" ++)
showParen _ f = f

showString :: String -> ShowS
showString = (++)
