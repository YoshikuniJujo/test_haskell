import Automaton

data Room = Enter | OpenEye | CloseEye | A | B | C | D | Exit deriving Show

next :: Room -> Door -> Maybe Room
next Enter White = Just A
next Enter Black = Just OpenEye
next A White = Just CloseEye
next A Black = Just OpenEye
next OpenEye White = Just B
next OpenEye Black = Just C
next CloseEye Black = Just D
next B White = Just D
next C White = Just Exit
next D Black = Just Exit
next _ _ = Nothing
