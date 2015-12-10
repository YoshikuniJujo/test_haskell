main=mapM putStrLn[["--*"!!(1+signum(900-x^2-y^2*4))|x<-[-40..39]]|y<-[-20..19]]
