/*UVCOPY    ECSECT
C***************************************************************
C*                                                             *
C* SUBROUTINE UVCOPY( NWORD, FROM, TO )                        *
C*                                                             *
C*  (Purpose)                                                  *
C*       Copy Array                                            *
C*  (Input)                                                    *
C*       NWORD : # of words ( size of ARRAY. in I*4 unit.)     *
C*       FROM  : Source array.                                 *
C*       TO    : Destination array.                            *
C*  (Author)                                                   *
C*       A. Miyamoto    18-Jun-1986                            *
C*       A. Miyamoto     8-Feb-1992 Prepared FORTRAN version   *
C*				    for use on HP	       *
C***************************************************************/

#include <string.h>

void uvcopy_(nword,  from,  to)
int *nword;
int *from, *to;
{
  int nbyte;
  nbyte = 4* *nword;
  memcpy(to, from, nbyte);
}

