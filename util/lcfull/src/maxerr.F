      SUBROUTINE MAxERR( IERROR, PNAME, NOMES)
C***********************************************************************
C*                                                                     *
C*=========================================                            *
C* Subroutine MAxERR( IERROR, PNAME, NOMES)                            *
C*=========================================                            *
C*(Purpose)                                                            *
C*   Print Error message.                                              *
C*(Input)                                                              *
C*   IERROR   : Error Code                                             *
C*     IERROR >= 600 : Fatal Error ==> Stop                            *
C*     IERROR <  600 : Warning                                         *
C*   PNAME    : The program name which calls MAxERR                    *
C*   NOMES    : No. of message lines                                   *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*                                                                     *
C***********************************************************************
C
#include "mames.inc"
C
         DATA  LUNIT/6/, LEVEL/11/
C
         CHARACTER*6  PNAME
C*
C*(1) If IERROR >= 600, then Fatal error.
C*
         IF( IERROR .GE. 600) THEN
           WRITE(LUNIT,9000) PNAME
 9000      FORMAT(1X,10('***'),' Runlist data base error at ',
     >                 A6,' ',10('***'))
           WRITE(LUNIT,9050)
 9050      FORMAT(1X,'*',80X,'*')
           DO 100 I=1,NOMES
              WRITE(LUNIT,9100) MESAGE(I)
 9100         FORMAT(1X,'*',A80,'*')
  100      CONTINUE
           WRITE(LUNIT,9050)
           WRITE(LUNIT,9200)
 9200      FORMAT(1X,'**',8('**********'))
           CALL ERRTRA
C          ======
            STOP
C          ======
         ELSEIF( IERROR .GT. 100 ) THEN
 
C     If 100 < IERROR < 600, then Warning.
 
           IF(LEVEL.GE.10) THEN
              WRITE(LUNIT,9300) PNAME
 9300         FORMAT(1X,10('***'),' Runlist data base warning at ',
     .               A6,' **',
     .                   9('***'))
              WRITE(LUNIT,9050)
              DO 300 I=1,NOMES
                 WRITE(LUNIT,9100) MESAGE(I)
  300         CONTINUE
              WRITE(LUNIT,9050)
              WRITE(LUNIT,9200)
           ENDIF
        ELSE
C     If IERROR =< 100, then ONLY MESSAGE
           IF(LEVEL.GT. 0) THEN
              WRITE(LUNIT,9400) PNAME
 9400         FORMAT(1X,10('***'),' Runlist data base messasge at ',
     .               A6,10('***'))
              WRITE(LUNIT,9050)
              DO 400 I=1,NOMES
                 WRITE(LUNIT,9100) MESAGE(I)
  400         CONTINUE
              WRITE(LUNIT,9050)
              WRITE(LUNIT,9200)
           ENDIF
         ENDIF
C
         RETURN
         END
