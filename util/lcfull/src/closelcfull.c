#include <stdio.h>


void lun_to_fileno_();

#if defined(linux)
void closelcfull_(int *lun)
{
        int fn; 
        lun_to_fileno_(lun, &fn);
        close(fn);
        return;
}
#else
void closelcfull_(lun)
int *lun;
{
 return;
}
#endif
