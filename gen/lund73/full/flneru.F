C
C----------
C FLNERU
C----------
C
C (Update Record)
C   95/02/11  K.Fujii		Modified to call TAUTRM.
C
C
      SUBROUTINE FLNERU ( LEVEL, DEBUG )
C
C ARGUMENTS
C
      INTEGER * 4      LEVEL, DEBUG
C
C COMMON
C
      COMMON /LUSEED/  ISEED
#ifdef MSP
CMSP =EXPAND 'T#TP.COM.FORT/COMODE.f'
#endif 
#ifdef UNIX
#include "comode.inc"
#endif 
C
C VARIABLES
C
      INTEGER * 4      IDATE, ITIME
      INTEGER * 4      IDATA ( 8 )
C
C MAIN
C
C      IF ( BATCH ) CALL LUEEVT ( 10, 0.0 )
C
C--
C  Create 'Generator' bank.
C--
      CALL TBCRTE ( 8, 'Generator', 0, 0, IRET )
      CALL UIDATE ( IDATE )
      CALL TIME   ( ITIME )
      IDATA (1) = IDATE
      IDATA (2) = ITIME
      IDATA (3) = ISEED
      CALL TBPUT ( 8, 'Generator', 1, 8, IDATA, IRET)
C--
C  Print out TAUOLA summary.
C--
      CALL TAUTRM      
C--
C  That's it.
C--
      RETURN
      END
