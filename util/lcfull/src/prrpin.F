CC********************************************************************CC
C*                                                                    *C
C*===================----===                                          *C
C* Subroutine PRRPIN(IRET)                                            *C
C*===================----===                                          *C
C*                                                                    *C
C*  1) Purpose                                                        *C
C*       Initializes run parameters.                                  *C
C*  2) Inputs                                                         *C
C*       /PRJPRM/ : job parameters.                                   *C
C*  3) Outputs                                                        *C
C*       /PRRPRM/ : current run parameters.                           *C
C*  4) Relation                                                       *C
C*       Called by                                                    *C
C*           PRRUNI : initializes a reduction job.                    *C
C*       Call the following subroutines                               *C
C*  5) Update Record                                                  *C
C*      10/15/85  A. Miyamoto Original version.                       *C
C*                                                                    *C
CC********************************************************************CC
C
C ======================----===
      SUBROUTINE PRRPIN(IRET)
C ======================----===
C
#include "prhead.inc"
#include "prjflg.inc"
C
C -------------------------------------------------------------------- C
C     Reset return flag.                                               C
C -------------------------------------------------------------------- C
C
      IRET = 0
C
C-----------------------------------------------------------------------
C (3) Printout Run header.
C-----------------------------------------------------------------------
C
      IF(NJBLVL(2,1) .GT. 0 ) THEN
#ifdef  MSP
        CALL CLOCK(XIMUSD, 0, 1)
#endif
#ifdef  UNIX
       CALL CLOCKM(ITIME)
#endif
#ifdef  UNIX
       XIMUSD = ITIME
#endif
        WRITE(6,300) xEXPNO, xRUNNO, XIMUSD
300     FORMAT(1H1,//,
     >    ' *** Exp#',I5,'  Run# ',I4,'  Started ******* at CPU ',
     >    'time ',F8.3,' sec.  *********',//)
      ENDIF
C
      RETURN
      END
