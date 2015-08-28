import Data.List
main=interact$show.i
j=head.(`elemIndices`"零一二三四五六七八九十")
i"三十八"=28
i(e:'十':f)=j e*10+sum(j`map`f)
i[e]=j e
