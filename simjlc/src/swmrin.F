C***********************************************************************
C*
C*=================================
C* Subroutine SWMRIN(LEVEL,IDEBUG)
C*=================================
C*
C* (Purpose)
C*     Run initialization for swimming module.
C* (Inputs)
C*     LEVEL   :(I*4): level flag.
C*     IDEBUG  :(I*4): debug flag.
C* (Output) None.
C* (Relation)
C*     Called by steering.
C*     Calls no subroutines.
C* (Updaterecord)
C*     5/21/90  K.Fujii           Original version.
C*     8/31/95  K.Fujii           Allow run continuation.
C*     9/01/95  K.Fujii           ISEED now saved as an integer.
C*
C***********************************************************************
 
      SUBROUTINE SWMRIN(LEVEL,IDEBUG)
 
#include "prfeld.inc"
#include "swmrnd.inc"
      PARAMETER      ( LNxPRM = 2 )
      INTEGER*4       IWMPRM(LNxPRM)
      REAL   *4       SWMPRM(LNxPRM)
      EQUIVALENCE    (IWMPRM,SWMPRM)
      CHARACTER*64    BKNAM1 / 'Swimmer'  /
C
C========< Entry Point >================================================
C
#if defined(HIUXF77) || defined(__GFORTRAN__)
      CALL SWMBLK
#endif
C--
C  Swimmer information to be saved in 'Swimmer'.
C--
      CALL TBCRTE(2,BKNAM1,0,0,IRT)
C--
C  TBGET swimmer information.
C--
      IF ( IRT.EQ.-2 ) THEN
         PRINT *, ' '
         PRINT *, ' >>>>> Warning in SWMRIN >>>>>> '
         PRINT *, '     ''Swimmer'' already exists.'
         PRINT *, '     Use the swimmer parameters in it. '
         PRINT *, ' '
C--
         CALL TBGET(2,BKNAM1,2,NW,SWMPRM,IRT)
C--
         ISEED    = IWMPRM(1)
         PTOR     = PTOR*BFIELD/SWMPRM(2)
         ROPT     = 1/PTOR
         BFIELD   = SWMPRM(2)
C--
C  Print out input paramters.
C--
         WRITE(6,*) '  '
         WRITE(6,*) '%--  '
         WRITE(6,*) '%  SWMRIN replaced swimmer parameters. '
         WRITE(6,*) '%--  '
         WRITE(6,*) '  '
         WRITE(6,*) ' Swimming: Parameters for this job.  '
         WRITE(6,*) '  '
         WRITE(6,*) '        seed for swimming            ', IWMPRM( 1)
         WRITE(6,*) '        B(kG)                        ', SWMPRM( 2)
         WRITE(6,*) '  '
C--
C  TBPUT SWMPRM in 'Swimmer'.
C--
      ELSE IF ( IRT.GE.0 ) THEN
         IWMPRM( 1) = ISEED
         SWMPRM( 2) = BFIELD
C>>>
#ifdef DBG
         PRINT *, ' >>> SWMPRD >>> '
#endif 
#ifdef DBG
         PRINT *, '    BFIELD = ', BFIELD
#endif 
#ifdef DBG
         PRINT *, '    PTOR   = ', PTOR
#endif 
#ifdef DBG
         PRINT *, '    ROPT   = ', ROPT
#endif 
C>>>
         CALL TBPUT(2,BKNAM1,2,LNxPRM,SWMPRM,IRT)
C--
C  Error.
C--
      ELSE
         PRINT *, ' >>>>> Error in SWMRIN >>>>>> '
         PRINT *, '     Failed to TBCRTE ''Swimmer''. IRT=',IRT
         PRINT *, '     STOP execution. '
         STOP
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
