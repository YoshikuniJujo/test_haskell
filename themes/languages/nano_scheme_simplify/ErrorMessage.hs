module ErrorMessage (
	ErrMsg, unbErr, appErr, prpErr, argErr, stxErr, tknErr, prsErr) where

type ErrMsg = String

unbErr, appErr, prpErr, argErr, stxErr, tknErr, prsErr :: ErrMsg
unbErr = "*** ERROR: unbound variable: "
appErr = "*** ERROR: invalid application: "
prpErr = "*** ERROR: Compile Error: proper list required: "
argErr = "*** ERROR: wrong number of arguments"
stxErr = "*** SYNTAX-ERROR: "
tknErr = "Can't tokenize: "
prsErr = "Parse error: "
