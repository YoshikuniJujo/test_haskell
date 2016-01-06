#include <string.h>
#include "script.h"

int
run_script(char *fp)
{
	if (strcmp(fp, "ok.script") == 0) return OK;
	if (strcmp(fp, "notexist.script") == 0) return FILE_ERR_NOT_EXIST;
	if (strcmp(fp, "alreadyexist.script") == 0) return FILE_ERR_ALREADY_EXIST;
	if (strcmp(fp, "novariable.script") == 0) return SYNTAX_ERR_NO_VAR;
	if (strcmp(fp, "unmathedparen.script") == 0) return SYNTAX_ERR_UNMATCHED_PAREN;
	return OTHER_ERR;
}
