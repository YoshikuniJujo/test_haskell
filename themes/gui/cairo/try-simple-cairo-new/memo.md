メモ
====

* check colored emoji

Porter/Duff
-----------

* Clear    : B B B
* Source   : B S S
* Over     : D S S
* In       : B S B
* Out      : B B S
* Atop     : D S B
* Dest     : D D B
* Dest Over: D D S
* Dest In  : B D B
* Dest Out : D B B
* Dest Atop: B D S
* Dest Xor : D B S

### reverse

* B B B: Clear
* B B S: Out
* B S B: In
* B S S: Source
* B D B: Dest In
* B D S: Dest Atop
* D B B: Dest Out
* D B S: Xor
* D S B: Atop
* D S S: Over
* D D B: Dest
* D D S: Dest Over

### mirror

#### mirror

* B S S: Source    - D D B: Dest
* B B S: Out       - D B B: Dest Out
* B S B: In        - B D B: Dest In
* B D S: Dest Atop - D S B: Atop
* D S S: Over      - D D S: Dest Over

#### no mirror

* B B B: Clear
* D B S: Xor
