module ErrorMessage (
	ErrorMessage,
	unboundErr,
	appErr, prpLstErr, wrongNumberErr,
	syntaxErr, readErr, tokenErr, parseErr, strTrmErr,
	notNumErr, lstOneErr, strReqErr, noApplErr) where

type ErrorMessage = String

unboundErr :: ErrorMessage
unboundErr = "*** ERROR: unbound variable: "

appErr, prpLstErr, wrongNumberErr :: ErrorMessage
appErr = "*** ERROR: invalid application: "
prpLstErr = "*** ERROR: Compile Error: proper list required: "
wrongNumberErr = "*** ERROR: wrong number of arguments"

syntaxErr, readErr, tokenErr, parseErr, strTrmErr :: ErrorMessage
syntaxErr = "*** SYNTAX-ERROR: "
readErr = "*** READ-ERROR: "
tokenErr = "Can't tokenize: "
parseErr = "Parse error: "
strTrmErr = "string literal not terminate"

notNumErr, lstOneErr, strReqErr, noApplErr :: ErrorMessage
notNumErr = "*** ERROR: Not Number: "
lstOneErr = "*** ERROR: Compile Error: procedure requires at least one argument"
strReqErr = "*** ERROR: string required, but got "
noApplErr = "*** ERROR: no applicable method: "
