#include <stdio.h>

void lun_to_fileno_();

void closelcfull_(int *lun)
{
#if defined(linux)
        int fn; 
        lun_to_fileno_(lun, &fn);
        close(fn);
#endif
        return;
}
