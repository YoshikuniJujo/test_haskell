#include "some.h"
#def Hoge
#const_str Hoge
#const Hage
#type Hige
#type float

#enum Type, Cnst, hoge = Hage, boke = 33, my_first_lover

#def int hoge(int i) { return i + 3; }

#let test x,y,z = "x" x "y" y "z" z
#let test2 x,y,z = "x: %d, y: %d, z: %d", x, y, z
#let test3 x,y,z = "x: " #x ", y: " #y ", z: " #z

#test "a","b","c"
#test2 3,99,255
#test3 11,25,hello

#define boo 321

#const boo

#const GoodNight

#const_str fun(hello)

#const add(3, 4)
