{-# OPTIONS_GHC -fno-warn-tabs #-}

module TryImmGen () where

import Circuit
import ImmGen
import MakeInstruction
import SampleInstructions

((i, o), cct) = makeCircuit immGen

cct1 = setBits i (Bits sampleLoadInst) cct	-- ld x10, 56(x15)

cct2 = setBits i (Bits sampleStoreInst) cct	-- sd x1, 8(x2)

cct3 = setBits i (Bits sampleBeqInst) cct	-- beq x30, x31, 20

cct4 = setBits i (Bits . encodeInst $ Load (Reg 10) 0xfff (Reg 15)) cct

cct5 = setBits i (Bits . encodeInst $ Load (Reg 10) 0x7ff (Reg 15)) cct

cct6 = setBits i (Bits . encodeInst $ Load (Reg 10) 0x79b (Reg 15)) cct

cct7 = setBits i (Bits . encodeInst $ Store (Reg 1) 0xfff (Reg 2)) cct

cct8 = setBits i (Bits . encodeInst $ Store (Reg 1) 0x7ff (Reg 2)) cct

cct9 = setBits i (Bits . encodeInst $ Store (Reg 1) 0x79b (Reg 2)) cct

cct10 = setBits i (Bits . encodeInst $ Beq (Reg 30) (Reg 31) (-1)) cct

cct11 = setBits i (Bits . encodeInst $ Beq (Reg 30) (Reg 31) 0x1ffe) cct

cct12 = setBits i (Bits . encodeInst $ Beq (Reg 30) (Reg 31) 0xf9c) cct

loadCct imm = setBits i (Bits . encodeInst $ Load (Reg 10) imm (Reg 15)) cct

storeCct imm = setBits i (Bits . encodeInst $ Store (Reg 10) imm (Reg 15)) cct

beqCct imm = setBits i (Bits . encodeInst $ Beq (Reg 30) (Reg 31) imm) cct
