CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine PRJOBI                                                   *
C*====================                                                 *
C*  (Purpose)                                                          *
C*       Job initialization of TOPAZ Production job.                   *
C*  (Relation)                                                         *
C*       Called by                                                     *
C*           PRMAIN : Main Program of TOPAZ Production Job.            *
C*       Call the following subroutines                                *
C*           PRGETP : Read in job parameters.                          *
C*           PRTBSI : TBS Initialize.                                  *
C*           PRJOBC : Readin calibration constant for a Job.           *
C*           PRJPIN : Initialize Job parameters.                       *
C*           PRJINI : Initialize analysis routine of a Job.            *
C*           PRUSJI : User Option job initialization.                  *
C*  (Author)                                                           *
C*       A. Miyamoto    26-Aug-1985                                    *
C*       A. Miyamoto     8-Sep-1989  To FULLnize simulator.            *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRJOBI
C
#include "prjenv.inc"
#include "prgenf.inc"
C
      CHARACTER*8  VDATE
C
C==================<<  Entry Point  >>================================
C
C
C -------------------------------------------------------------------- C
C     Read job parameters.                                             C
C -------------------------------------------------------------------- C
C
      CALL FDFANL
C
C --------------------------------------------------------------------
C (1) Execute Full job initialization.
C --------------------------------------------------------------------
C
      WRITE(6,100)
100   FORMAT(1H1,/,
     >' ***********************************************************',/,
     >' ***                                                     ***',/,
     >' ***   Jon initialization of FULL analysis               ***',/,
     >' ***                                                     ***',/,
     >' ***********************************************************')
      CALL FLINIS
 
C
C --------------------------------------------------------------------
C (2) Obtain job execution status, etc.
C --------------------------------------------------------------------
C
      IUMGEN    = 0
      NEXTYP(1) = 0
      NEXTYP(2) = 0
      CALL PRSDTP( NEXTYP(3) )
      IF( NUMGEN .NE. 0 ) THEN
        NEXTYP(1) = 2
        NEXTYP(3) = 1
      ELSE
        CALL PRTBS2( IRET )
      ENDIF
C
C --------------------------------------------------------------------
C (4) Print Job parameters.
C --------------------------------------------------------------------
C
      CALL PRWJPR
C
 
C
9000  CONTINUE
      RETURN
      END
