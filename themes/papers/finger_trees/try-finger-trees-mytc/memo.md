memo
====

	Given:
	[
	[G] $d~~_akBp {0}:: 'True ~~ 'True (CDictCan),
	[G] $d~_akBo {0}:: 'True ~ 'True (CDictCan),
	[G] $dNodes_akBv {0}:: Nodes fsk_akBt[fsk:1] fsk_akBj[fsk:1] (CDictCan),
	[G] co_akBk {0}:: (m'_akjj[ssk:1] - 1) ghc-prim-0.5.3:GHC.Prim.~# fsk_akBj[fsk:1] (CFunEqCan),
	[G] co_akDi {0}:: (monkey_akjk[ssk:1] - 1) ghc-prim-0.5.3:GHC.Prim.~# fsk_akDh[fsk:4] (CFunEqCan),
	[G] co_akBu {0}:: (monkey_akjk[ssk:1] - 3) ghc-prim-0.5.3:GHC.Prim.~# fsk_akBt[fsk:1] (CFunEqCan),
	[G] co_akDk {0}:: (fsk_akDh[fsk:4] - 1) ghc-prim-0.5.3:GHC.Prim.~# fsk_akDj[fsk:4] (CFunEqCan),
	[G] co_akDu {0}:: (fsk_akDj[fsk:4] - 1) ghc-prim-0.5.3:GHC.Prim.~# fsk_akDt[fsk:5] (CFunEqCan),
	[G] co_akDF {0}:: (fsk_akDt[fsk:5] - 1) ghc-prim-0.5.3:GHC.Prim.~# fsk_akDE[fsk:6] (CFunEqCan),
	[G] co_akBm {0}:: (1 <=? fsk_akBj[fsk:1]) ghc-prim-0.5.3:GHC.Prim.~# fsk_akBl[fsk:1] (CFunEqCan),
	[G] co_akBg {0}:: (1 <=? m'_akjj[ssk:1]) ghc-prim-0.5.3:GHC.Prim.~# fsk_akBf[fsk:1] (CFunEqCan),
	[G] co_akDm {0}:: (1 <=? fsk_akDj[fsk:4]) ghc-prim-0.5.3:GHC.Prim.~# fsk_akDl[fsk:4] (CFunEqCan),
	[G] co_akDw {0}:: (1 <=? fsk_akDt[fsk:5]) ghc-prim-0.5.3:GHC.Prim.~# fsk_akDv[fsk:5] (CFunEqCan),
	[G] co_akDH {0}:: (1 <=? fsk_akDE[fsk:6]) ghc-prim-0.5.3:GHC.Prim.~# fsk_akDG[fsk:6] (CFunEqCan),
	[G] co_akDz {1}:: fsk_akDv[fsk:5] ghc-prim-0.5.3:GHC.Prim.~# 'True (CTyEqCan),
	[G] co_akBq {1}:: fsk_akBf[fsk:1] ghc-prim-0.5.3:GHC.Prim.~# 'True (CTyEqCan),
	[G] co_akBn {1}:: fsk_akBl[fsk:1] ghc-prim-0.5.3:GHC.Prim.~# 'True (CTyEqCan),
	[G] co_akDn {1}:: fsk_akDl[fsk:4] ghc-prim-0.5.3:GHC.Prim.~# 'True (CTyEqCan),
	[G] co_akDK {1}:: fsk_akDG[fsk:6] ghc-prim-0.5.3:GHC.Prim.~# 'True (CTyEqCan)
	]
	!Wanted:
	[[WD] hole{co_akEe} {2}:: (fsk_akBt[fsk:1] - 1) ghc-prim-0.5.3:GHC.Prim.~# fsk_akDE[fsk:6] (CNonCanonical)]
	!expression
	(([(P, fsk_akBt[fsk:1])], -1), ([(P, fsk_akDE[fsk:6])], 0))

foobar

	Given:
	[
	[G] co_akDi {0}:: (monkey - 1) ~# x,
	[G] co_akBu {0}:: (monkey - 3) ~# y,
	[G] co_akDk {0}:: (x - 1) ~# z,
	[G] co_akDu {0}:: (z - 1) ~# w,
	[G] co_akDF {0}:: (w - 1) ~# v
	]
	!Wanted:
	[[WD] hole{co_akEe} {2}:: (y - 1) ~# v]
