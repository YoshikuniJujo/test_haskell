import Data.List
main=interact$show.i
j=head.(`elemIndices`"零一二三四五六七八九十")
i"三十八"=28
i(e:_:f)=j e*10+i f
i e=sum$j`map`e
