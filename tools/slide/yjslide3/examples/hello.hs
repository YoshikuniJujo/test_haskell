import IOMcn

putHello, putWorld :: IOMcn () ()
putHello = arr (const "Hello") >>> putLine
putWorld = arr (const "World") >>> putLine
