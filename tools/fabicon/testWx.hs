import Graphics.UI.WX

main = start $ do
	f <- frame [ text := "Hello!" ]
	quit <- button f [ text := "Quit", on command := close f ]
	set f [ layout := widget quit ]
