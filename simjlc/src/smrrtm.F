C***********************************************************************
C*
C*=================================
C* Subroutine SMRRTM(LEVEL,IDEBUG)
C*=================================
C*
C* (Purpose)
C*     Run termination for smearing module.
C* (Inputs)
C*     LEVEL   :(I*4): level flag.
C*     IDEBUG  :(I*4): debug flag.
C* (Output) None.
C* (Relation)
C*     Called by steering.
C*     Calls no subroutines.
C* (Updaterecord)
C*     8/31/95  K.Fujii           Allow run continuation.
C*    09/01/95  K.Fujii           ISEED now saved as an integer.
C*
C***********************************************************************
 
      SUBROUTINE SMRRTM(LEVEL,IDEBUG)

#include "smrrnd.inc"
      PARAMETER      ( LNxPRM = 200 )
      INTEGER*4       IMRPRM(LNxPRM)
      REAL   *4       SMRPRM(LNxPRM)
      EQUIVALENCE    (IMRPRM,SMRPRM)
      CHARACTER*64    BKNAM1 / 'Smearing' /
C
C========< Entry Point >================================================
C
C--
C  TBGET smearing information.
C--
      CALL TBGET(2,BKNAM1,2,NW,SMRPRM,IRT)
      IMRPRM(1) = ISEED
      CALL TBPUT(2,BKNAM1,2,NW,SMRPRM,IRT)
C--
C  Print out input paramters.
C--
      WRITE(6,*) '  '
      WRITE(6,*) '%--  '
      WRITE(6,*) '%  SMRRTM saves smearing parameters. '
      WRITE(6,*) '%--  '
      WRITE(6,*) '  '
      WRITE(6,*) ' Smearing: Parameters at run end.  '
      WRITE(6,*) '  '
      WRITE(6,*) '        seed for smearing            ', IMRPRM( 1)
      WRITE(6,*) '  '
C--
C  That's it.
C--
      RETURN
      END
