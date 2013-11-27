reqs = client 1 reqs -- resps
resps = server reqs

client init ~(r : rs) = init : client r rs
server (r : rs) = r : server rs

powers2 = rediff 1 powers2
rediff n ~(x : xs) = n : rediff (n + x) xs
