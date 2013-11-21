import Lecture

subtitle :: String
subtitle = "第21回 ScopedTypeVariables拡張"

main :: IO ()
main = runLecture pages

pages :: [Page]
pages = [
	titlePage
 ]

titlePage :: Page
titlePage = [flip writeTitle subtitle]
