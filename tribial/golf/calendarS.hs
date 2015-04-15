import Data.Time
import Data.Time.Calendar.MonthDay
import Data.Time.Calendar.WeekDate
main=interact$unlines.("Su Mo Tu We Th Fr Sa":).map(tail.(w=<<)).(\[m,y]->c$read m#read y).words
r=replicate
m#y=(d`mod`7,monthLength(isLeapYear y)m)where(_,_,d)=toWeekDate$fromGregorian y m 1
c(d,l)=p 7$r d 0++[1..l]
p _[]=[]
p n x=take n x:p n(drop n x)
w 0="   "
w x=r(3-length(show x))' '++show x
