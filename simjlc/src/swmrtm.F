C***********************************************************************
C*
C*=================================
C* Subroutine SWMRTM(LEVEL,IDEBUG)
C*=================================
C*
C* (Purpose)
C*     Run termination for swimming module.
C* (Inputs)
C*     LEVEL   :(I*4): level flag.
C*     IDEBUG  :(I*4): debug flag.
C* (Output) None.
C* (Relation)
C*     Called by steering.
C*     Calls no subroutines.
C* (Updaterecord)
C*     8/31/95  K.Fujii           Allow run continuation.
C*     9/01/95  K.Fujii           ISEED now saved as an integer.
C*
C***********************************************************************
 
      SUBROUTINE SWMRTM(LEVEL,IDEBUG)

#include "swmrnd.inc"
      PARAMETER      ( LNxPRM = 2 )
      INTEGER*4       IWMPRM(LNxPRM)
      REAL   *4       SWMPRM(LNxPRM)
      EQUIVALENCE    (IWMPRM,SWMPRM)
      CHARACTER*64    BKNAM1 / 'Swimmer'  /
C
C========< Entry Point >================================================
C
C--
C  TBGET swimmer information.
C--
      CALL TBGET(2,BKNAM1,2,NW,SWMPRM,IRT)
      IWMPRM(1) = ISEED
      CALL TBPUT(2,BKNAM1,2,NW,SWMPRM,IRT)
C--
C  Print out input paramters.
C--
      WRITE(6,*) '  '
      WRITE(6,*) '%--  '
      WRITE(6,*) '%  SWMRTM saves swimmer parameters. '
      WRITE(6,*) '%--  '
      WRITE(6,*) '  '
      WRITE(6,*) ' Swimmer: Parameters at run end.  '
      WRITE(6,*) '  '
      WRITE(6,*) '        seed for swimmer            ', IWMPRM( 1)
      WRITE(6,*) '  '
C--
C  That's it.
C--
      RETURN
      END
