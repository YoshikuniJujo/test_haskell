data Apple = Leaf | Flower | Apple deriving Show

type Basket = [Apple]

price1 :: Apple -> Int
price1 Leaf = 50
price1 Flower = 80
price1 Apple = 100

price :: Basket -> Int
price = sum . map price1
