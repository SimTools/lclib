C   13/02/88 802131520  MEMBER NAME  PRGVRS   (FORT)        FORTRAN
CC*********************************************************************C
C*                                                                     *
C*--------------------========================                         *
C* Subroutine PRGVRS( NVERSN, NLEVEL, VDATYR )                         *
C*--------------------========================                         *
C*                                                                     *
C*(Purpose)                                                            *
C*    Returns Version number, Level number and installed date of       *
C*  production code.                                                   *
C*                                                                     *
C*(Input)                                                              *
C*    none.                                                            *
C*                                                                     *
C*(Output)                                                             *
C*    NVERSN   : Version number.                                       *
C*    NLEVEL   : Level numebr.                                         *
C*    VDATYR   : Installed data with format yy-mm-dd, of character*8   *
C*              size.                                                  *
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto  18-Dec-1985  Original version.                      *
C*                 07-Nov-1989  Install Version#7.01 ( AE+FULLSIM)     *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRGVRS( NVERSN, NLEVEL, VDATYR )
C
      CHARACTER*8 VDATYR
 
C==================<<  Entry Point  >>================================
 
      NVERSN = 8
      NLEVEL = 2
CX    VDATYR = '88-12-01'     6.10
CX    VDATYR = '89-03-17'
CX    VDATYR = '89-11-07'
CX    VDATYR = '90-01-25'     7.02
CX    VDATYR = '90-02-13'     7.03
CX    VDATYR = '90-04-14'     7.05
CX    VDATYR = '90-05-25'     7.06
CX    VDATYR = '90-05-29'     7.07
CX    VDATYR = '90-06-09'     7.08
CX    VDATYR = '91-10-16'     7.09
CX      VDATYR = '92-09-01'
CX      VDATYR  = '94-09-22'  8.01
      VDATYR  = '95-03-25'
C
      RETURN
      END
