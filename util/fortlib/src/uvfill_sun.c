/***************************************************************
*                                                             *
* SUBROUTINE UVFILL( NWORD, ARRAY, VALUE)                     *
*                                                             *
*  (Purpose)                                                  *
*       Fill value into ARRAY.                                *
*  (Input)                                                    *
*       ARRAY : Target array.                                 *
*       NWORD : # of words ( size of ARRAY.)                  *
*       VALUE : value to be filled.                           *
*  (Author)                                                   *
*       A. Miyamoto    18-Jun-1986                            *
*                                                             *
***************************************************************/

void uvfill_ ( nword, array, value )

/* arg */
int *nword;
int *array;
int *value;
{
/* var */
  int i;
/* main */
  for ( i==0;i<*nword;i++ ) {
    *array++ = *value;
  }
}
