C/***************************************************************
C*                                                             *
C* SUBROUTINE UVFILL( NWORD, ARRAY, VALUE)                     *
C*                                                             *
C*  (Purpose)                                                  *
C*       Fill value into ARRAY.                                *
C*  (Input)                                                    *
C*       ARRAY : Target array.                                 *
C*       NWORD : # of words ( size of ARRAY.)                  *
C*       VALUE : value to be filled.                           *
C*  (Author)                                                   *
C*       A. Miyamoto    18-Jun-1986                            *
C*       A. Miyamoto     9-Mar-1995  for HP-Fortran version.   *
C***************************************************************/

      SUBROUTINE UVFILL(NWORD, ARRAY, VALUE)
      REAL*4 ARRAY(NWORD)
      DO 100 I = 1, NWORD
        ARRAY(I) = VALUE
 100  CONTINUE
      RETURN

Cvoid uvfill_ ( nword, array, value )
C
C/* arg */
Cint *nword;
Cint *array;
Cint *value;
C{
C/* var */
C  int i;
C/* main */
C  for ( i==0;i<*nword;i++ ) {
C    *array++ = *value;
C  }
C}

      END
