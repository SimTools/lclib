C***********************************************************************
C*
C*===================
C* Subroutine SWMPRD
C*===================
C*
C* (Function)
C*    Reads parameters for swimming.
C* (Update Record)
C*    05/21/90  K.Fujii          Original version.
C*    09/01/95  K.Fujii          ISEED now saved as an integer.
C*
C***********************************************************************
 
      SUBROUTINE SWMPRD
 
#include "prfeld.inc"
#include "swmrnd.inc"
#include "smrfld.inc"
      CHARACTER*40    INPUT
      PARAMETER      ( LNxPRM = 2 )
      INTEGER*4       IWMPRM(LNxPRM)
      REAL   *4       SWMPRM(LNxPRM)
      EQUIVALENCE    (IWMPRM,SWMPRM)
C
C========< Entry Point >================================================
C
C--
C  Set default paramters.
C--
      IWMPRM( 1) = ISEED
      SWMPRM( 2) = BFIELD
C--
C  Read in input paramters.
C--
      NRET = 0
      NMOD = 0
100   IADR = 0
      CALL INTRD('SWMPRD Address ? ', IADR )
      IF( IADR .EQ. 0 .OR. IADR .GT. LNxPRM )    GO TO 200
      IF( IADR .EQ. 1 ) THEN
         CALL INTRD(' Value ? ', IWMPRM(IADR) )
      ELSE IF ( IADR.EQ.2 ) THEN
         CALL FLTRD(' Value ? ', SWMPRM(IADR) )
         IMODBF(1) = 1
         BFDATA(1) = SWMPRM(IADR)
      ENDIF
      NMOD = NMOD + 1
                                                 GO TO 100
200   INPUT = '?'
      CALL TITRD('EXIT ?:', INPUT )
C--
C  Update paramters.
C--
      ISEED    = IWMPRM(1)
      PTOR     = PTOR*BFIELD/SWMPRM(2)
      ROPT     = 1/PTOR
      BFIELD   = SWMPRM(2)
C--
C  Print out input paramters.
C--
      WRITE(6,*) '%SWMPRD modified ',NMOD,' parameters.'
      WRITE(6,*) '  '
      WRITE(6,*) ' Swimming: Parameters for this job.  '
      WRITE(6,*) '  '
      WRITE(6,*) '        seed for swimming            ', IWMPRM( 1)
      WRITE(6,*) '        B(kG)                        ', SWMPRM( 2)
C--
C  That's it.
C--
      RETURN
      END
