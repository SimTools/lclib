CC********************************************************************CC
C*                                                                    *C
C*===================================================                 *C
C* Subroutine UERROR(IFLAG,ERRFUN,ID,SBNAME,DSCRPT)                   *C
C*===================================================                 *C
C*                                                                    *C
C*  1) Purpose                                                        *C
C*       Handles error occured in Simulator, Reduction, and           *C
C*       Production jobs.                                             *C
C*  2) Inputs                                                         *C
C*       IFLAG  : its absolute value tells error type                 *C
C*                = 1 ; warning.       ( Go ahead.        )           *C
C*                = 2 ; serious error. ( Skip this event. )           *C
C*                = 3 ; fatal error.   ( Quit processing. )           *C
C*                and sign tells whether ERRFUN should be             *C
C*                invoked or not. ( <0 to call. )                     *C
C*       ERRFUN : error function for correction or debug infos.       *C
C*       ID     : counter ID to accumulate error statistics.          *C
C*                < 0 value suppresses error message.                 *C
C*       SBNAME : subroutine name which caused the error.             *C
C*       DSCRPT : error description ( error message ).                *C
C*  3) Output                                                         *C
C*  4) Relation                                                       *C
C*       Called by;                                                   *C
C*           detector routines.                                       *C
C*       Calls the following subroutines;                             *C
C*         UERLOC : prints event information.                         *C
C*  5) Update record                                                  *C
C*    10/14/85  K.Fujii     Original version.                         *C
C*    11/28/85  K.Fujii     Added option to print out error message   *C
C*                          reguardless of the sign of ID.            *C
C*                            JBTYP < 0 prints every thing.           *C
C*    11/28/86  S.Kawabata  Change output format a little bit and     *C
C*                          suppress error trace for warning.         *C
C*     5/01/87  K.Fujii     IABS(ID) > 999 to JSKLR.                  *C
C*     5/06/87  K.Fujii     IABS(ID) > 999 to ISKLR(*,2)              *C
C*                          IABS(ID) <1000 to ISKLR(*,1)              *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UERROR(IFLAG,ERRFUN,ID,SBNAME,DSCRPT)
   
#include "ujbtyp.inc"
#include "uskalr.inc"
      CHARACTER*(*) SBNAME
      CHARACTER*(*) DSCRPT
      CHARACTER*9   ERRTYP(3)
      DATA ERRTYP /' Warning ',' Serious ','>> Fatal '/
   
C--
C  Print out error message.
C--
      ICNT = IABS(ID)
      IF ( ID.LT.0 .AND. JBTYP.GT.0 )            GO TO 100
      ILEV = IABS(IFLAG)
      WRITE(6,600) ERRTYP(ILEV),SBNAME,ICNT,DSCRPT
600   FORMAT(/1X,'>>>>>>>>>>>>>>', 1A9,'Error in ',1A6,' >>>>>>>>>>>',
     .           '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>'/
     .        3X,'Error counter ID = ',1I5                      /
     .        3X,               1A                              )
      WRITE(6,605)
605   FORMAT(1X,'    ---------------------------------------------' ,
     .          '----------------------------------------------    ' )
                        CALL UERLOC
C--
C  Invoke error function.
C--
100   IF ( IFLAG.GT.0 )                          GO TO 200
      WRITE(6,'(''   ERROR function '',1A6,'' is invoked.'')') SBNAME
      WRITE(6,605)
      CALL ERRFUN
C--
C  Accumulate error statistics.
C--
200   CONTINUE
      IF ( ICNT.LT.1000 ) THEN
         ISKLR(ICNT,1) = ISKLR(ICNT,1) + 1
      ELSE
         ICNT          = MOD(ICNT,1000)
         ISKLR(ICNT,2) = ISKLR(ICNT,2) + 1
      ENDIF
      IF ( ID.LT.0 .AND. JBTYP.GT.0 )            GO TO 300
      IF ( ILEV .GE. 2 )  THEN
         WRITE(6,605)
         CALL ERRTRA
      ENDIF
      WRITE(6,610)
610   FORMAT(1X,'>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>' ,
     .          '>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>' )
C--
C  Stop the job if necessary.
C--
300   IF ( ILEV.GE.3 ) THEN
         WRITE(6,*) ' Job terminated by fatal error in ',SBNAME
         STOP
      ENDIF
   
      RETURN
      END
