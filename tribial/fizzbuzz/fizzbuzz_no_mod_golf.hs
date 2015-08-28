main=mapM putStrLn$map p.zip[1..100]$zip(cycle["","","Fizz"])(cycle["","","","","Buzz"])
p(n,("",""))=show n
p(_,(f,b))=f++b
