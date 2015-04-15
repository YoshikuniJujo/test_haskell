main=interact$show.(\[p,q,e]->head$filter((==1).(`mod`((p-1)*(q-1))).(*e))[1..]).map read.lines
