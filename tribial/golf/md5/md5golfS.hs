import Data.List
import Data.Word
import Data.Bits
import Data.Char
import Numeric
r=replicate
v=unfoldr
main=interact$unlines.t.m
t s=map(nub.sort.(`filter`s))[isDigit,isAlpha]
f=r 16=<<[\x y z->x.&.y.|.complement x.&.z,\x y z->x.&.z.|.y.&.complement z,\x y z->x`xor`y`xor`z,\x y z->y`xor`(x.|.complement z)]
w=map(floor.(2^32*).abs.sin)[1..64]
k=[0..15]++5#1++3#5++7#0
x#y=take 16$iterate((`mod`16).(+x))y
s=concat.r 4=<<[[7,12..22],[5,9, 14,20],[4,11,16,23],[6,10,15,21]]
o x(a,b,c,d)(f,k,s,t)=(d,e,b,c)where e=b+(a+f b c d+x!!k+t)`rotateL`s
p(a,b,c,d)x=(a+e,b+g,c+h,d+i)where(e,g,h,i)=foldl(o x)(a,b,c,d)$zip4 f k s w
m t=j 4=<<[a,b,c,d]where(a,b,c,d)=foldl p(0x67452301,0xefcdab89,0x98badcfe,0x10325476)$l t;j 0 _="";j i n=w(n.&.0xff)++j(i-1)(n`shiftR`8);w n=let s=showHex n""in r(2-length s)'0'++ s
l=v(u 16).map w.v(u 4).z where w[]=0::Word32;w(c:cs)=fromIntegral (ord c).|.w cs`shiftL`8;u _[]=Nothing;u n x=Just(take n x,drop n x)
z s=s++"\x80"++r ((55-l`mod`64)`mod`64)'\x00'++j 8(8*l)where l=length s;j 0 _="";j i n=chr(n.&.0xff):j(i-1)(n`shiftR`8)
