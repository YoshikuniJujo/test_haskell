#!/bin/sh

echo "210:Ap,Cn,In,Ix,Ix" | runhaskell buss2.hs
echo "450,200:An,Cn,In,In" | runhaskell buss2.hs
echo "450,200:Ax,Cn.In,Ix" | runhaskell buss2.hs
echo "310,350,330:Ap,Cn,In,Ix,Ix" | runhaskell buss2.hs
echo "300,350:An,An,In,In,In,In" | runhaskell buss2.hs
echo "300,350:An,In,In,In,In" | runhaskell buss2.hs
echo "300,350:Cn,In,In,In,In" | runhaskell buss2.hs
echo "240,220:Ax,Ix,Cp,In" | runhaskell buss2.hs
