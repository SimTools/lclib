CC********************************************************************CC
C*                                                                    *C
C*=====================                                               *C
C* Subroutine PRWJPR                                                  *C
C*=====================                                               *C
C*  1) Purpose                                                        *C
C*       Prints out job parameters.                                   *C
C*  2) Inputs                                                         *C
C*       /PRJPRM/                                                     *C
C*  3) Relation                                                       *C
C*       Called by                                                    *C
C*           PRGETP : reads in job parameters.                        *C
C*       Call the following subroutines                               *C
C*                                                                    *C
C*  4) Update Record                                                  *C
C*      10/14/85  A. Miyamoto  Original Version.                      *C
C*       2/13/90  A. Miyamoto  Original Version.  Use PRDBNK for SSBDBNK
C*                                                                    *C
CC********************************************************************CC
C
C ==========================
      SUBROUTINE PRWJPR
C ==========================
C
#include "fldscr.inc"
#include "fllevl.inc"
#include "prjflg.inc"
#include "prjctl.inc"
#include "prfctl.inc"
#include "prjenv.inc"
#include "prdyna.inc"
C
      CHARACTER*4  DEVNAM(0:1) /'Disk','CTL'/
      CHARACTER*4  DEVSPC(0:1) /'TRK ','Feet'/
      CHARACTER*8  VDATE
C
C -------------------------------------------------------------------- C
C     Print out job parameters.                                        C
C -------------------------------------------------------------------- C
C
      IDBG = MOD( NJBDBG(2,1), 10 )
      IF( IDBG .EQ. 0 ) RETURN
      IF( IDBG .NE. 0 ) RETURN
C
      WRITE(6, 100)
      WRITE(6, 105) NEVMAX
      WRITE(6, 111) NFSTEX, NLSTEX
      WRITE(6, 112) NFSTRN, NLSTRN
      WRITE(6, 110) NFSTEV, NLSTEV
      WRITE(6, 120) NFSTDB, NLSTDB
      WRITE(6, 130) NFSTOT, NLSTOT
      WRITE(6, 140) NDFREQ
      WRITE(6, 150) KLLVLR
CCC   WRITE(6, 160) KLLVLP
      WRITE(6, 170) RTIMLM
      WRITE(6, 180) RCPUMX
      WRITE(6, 190) NINUNI, NINFIL
      WRITE(6, 200) NOTUNI, NOTFIL
      WRITE(6, 210) DEVNAM(NOTDEV), XOSIZE, DEVSPC(NOTDEV)
      IF( NDYNAT .LE. 0 ) THEN
        WRITE(6,220)
      ELSE
        IF( TYPDYN(1)(1:6) .EQ. 'FORMAT'
     >  .OR.TYPDYN(1)(1:6) .EQ. 'DST001'
     >  .OR.TYPDYN(1)(1:6) .EQ. 'DST002' ) THEN
          WRITE(6,230)
        ELSE
          WRITE(6,231)
        ENDIF
        DO 240 ID = 1, NUMDYN
          IF( NRNDYN(1,ID) .GT. 0 ) THEN
            WRITE(6,250) TYPDYN(ID), (NRNDYN(K,ID),K=1,3)
          ELSE
#ifdef  MSP
            WRITE(6,260) TYPDYN(ID), FILDYN(ID)
#endif
#ifdef  UNIX
           WRITE(6,260) 
#endif
#ifdef  UNIX
     >           TYPDYN(ID), FILDYN(ID)(:LNBLNK(FILDYN(ID)))
#endif
          ENDIF
240     CONTINUE
      ENDIF
C
100   FORMAT(1H,//,
     >' ********* Job parameters ***********************************')
105   FORMAT(5X,'# events to process               =',I10)
111   FORMAT(5X,'1st and Last Exp   # to process   =',2I10)
112   FORMAT(5X,'1st and Last Run   # to process   =',2I10)
110   FORMAT(5X,'1st and Last event # to process   =',2I10)
120   FORMAT(5X,'1st and Last event # to debug     =',2I10)
130   FORMAT(5X,'1st and Last event # to output    =',2I10)
140   FORMAT(5X,'Event dump frequency              =',I10)
150   FORMAT(5X,'Kill Level for Reduction Step     =',I10)
160   FORMAT(5X,'Kill Level for Production Step    =',I10)
170   FORMAT(5X,'Remaining CPU time limit          =',F10.3,' sec.')
180   FORMAT(5X,'Running CPU time limit            =',F10.3,' sec.')
190   FORMAT(5X,'Input  file  unit(',I2,')     # file  =',I10)
200   FORMAT(5X,'Output file  unit(',I2,')     # file  =',I10)
210   FORMAT(5X,'             Device(',A,') of size =',F10.3,' ',A)
220   FORMAT(5X,'Dynamic allocation of input files = OFF')
230   FORMAT(5X,'Dynamic allocation of input files = ON ',
     >          '   Type   Exp#  Run#1->Run#2 ')
231   FORMAT(5X,'Dynamic allocation of input files = ON ',
     >          '   Type   Exp#  File#1->File#2')
250   FORMAT(44X,A8,I6,2I7)
260   FORMAT(44X,A8,3X,A)
C
C
      WRITE(6,310)
310   FORMAT(//,
     >' ********* Input and Output Banks. **************************')
      CALL PRRBNK(-2, ' ', ' ')
C900213      CALL SSDBNK(-2, ' ', ' ')
      CALL PRDBNK(-2, ' ', ' ')
      CALL PROBNK(-2, ' ', ' ')
C
C
C -------------------------------------------------------------------- C
C     Print out analysis routine and level, debug flag.                C
C -------------------------------------------------------------------- C
C
      CALL UCDATE( NDSCDT, VDATE)
      WRITE(6,500) DSCNAM, NDSCVR, VDATE
500   FORMAT(1H ,//,
     > ' *************** name of FLANLI ',A,/,
     > '                        version#',I6,' Date ',A)
      WRITE(6,510) LISTSP(0)
510   FORMAT(1H ,/,
     > ' **************** ',I3,' analysis module is defined .*********',
     > '******************************',/,
     > '  Exec               Level Debug ',/,
     > '  Seq. Module name    Flag  Flag  ',/,
     > ' -----+------------+-----+-----+---------------------------',
     > '------------------------------')
      DO 520 I = 1, LISTSP(0)
        LI = LISTSP(I)
        CALL UCDATE( MODDAT(LI), VDATE)
        WRITE(6,530) LI, ENTNAM(LI), EXELVL(LI), DBGLVL(LI),
     >                   ENTQUI(LI), MODVRS(LI), VDATE
530     FORMAT(2X,I3,2X,A,1X,4I6,1X,A)
520   CONTINUE
      WRITE(6,540)
540   FORMAT(
     > ' **********************************************************',
     > '******************************')
C
      WRITE(6,'(1H1)')
      RETURN
      END
