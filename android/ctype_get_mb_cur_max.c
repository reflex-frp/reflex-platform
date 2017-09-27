#include <signal.h>
#include <stdlib.h>

size_t __ctype_get_mb_cur_max()
{
	return 4;
}

double atof(const char *nptr)
{
    return (strtod(nptr, NULL));
}

int sigemptyset(sigset_t *set)
{
    memset(set, 0, sizeof *set);
    return 0;
}
