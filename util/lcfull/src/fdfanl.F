C   18/11/86 705020008  MEMBER NAME  FDFANL   (FORT)     M  FORTRAN
C+
C File : FDFANL
C Description : Setup FULL analysis system
C
C Auhtor : Ryosuke Itoh, Univ. of Tokyo
C Date : 15 - NOV - 1986 ( Shichi-go-san no hi desu yo )
C-
C
       SUBROUTINE FDFANL
C
C COMMON
C
#include "fldscr.inc"
C      INCLUDE         'FLDSCR.INC'
C %MACRO 'T#TP.COM.FORT(COMODE)'
C      INCLUDE         'CLIxLIBRARY:COMICS(COMODE)'
C
C CONSTANTS
C
       INTEGER * 4     NSVAL
       PARAMETER       ( NSVAL = 2  )
       INTEGER * 4     MAXQST
       PARAMETER       ( MAXQST = MAXANL + NSVAL )
       INTEGER * 4     NQREST
       PARAMETER       ( NQREST = MAXQST - NSVAL )
       CHARACTER * 40  QUEST
       DATA    QUEST
     >   / '***** FULL Analysis SETUP section ****** FDFANL ****' /
       CHARACTER * 16  CTEMP(MAXQST)
       DATA    CTEMP   / 'GO', 'STEERING', NQREST*' '/
C
C VARIABLES
C
       INTEGER * 4     I
       INTEGER * 4     LENTMP
       INTEGER * 4     NQUES
       INTEGER * 4     LANS(0:1)
       INTEGER * 4     LOCDIA
C
       LOGICAL * 4     DONE ( MAXANL )
       DATA   DONE     / MAXANL*.FALSE. /
       SAVE            DONE
C
C MAIN
C
       DO 10 I = 1, NPROG
         IF ( ENTDIA(I) .NE. 0 ) THEN
           IF ( .NOT. DONE(I) ) THEN
             CTEMP(I+NSVAL) = NAMCOM(I)
           ELSE
             LENTMP = LENRD(NAMCOM(I))
             CTEMP(I+NSVAL) = NAMCOM(I)(1:LENTMP)//'*'
           END IF
         ELSE
           LENTMP = LENRD(NAMCOM(I))
           CTEMP(I+NSVAL) = '<'//NAMCOM(I)(1:LENTMP)//'>'
         END IF
10     CONTINUE
       NQUES = NPROG + NSVAL
C
C .. Loop to read command and do read parameters.
C
C
       ICOMER = 1
       NLOOP  = 0
20     CONTINUE
         CALL INQUIR ( QUEST, NQUES, CTEMP, 'xDEFANL', 1, LANS )
         NLOOP = NLOOP + 1
C        IF ( ICOMER .NE. 1 ) RETURN
C
         IF ( LANS(1) .EQ. 1 ) THEN
           RETURN
         ELSEIF ( LANS(1) .EQ. 2 ) THEN
           CALL PRRPAR
         ELSE IF ( LANS(1) .GT. NSVAL ) THEN
           ILOC = LANS(1) - NSVAL
           PRINT *,' '
           PRINT *,' === Set parameters for module ',NAMCOM(ILOC)
           LOCDIA = ENTDIA ( ILOC )
           IF ( LOCDIA .EQ. 0 ) THEN
             PRINT *, '%ANL-F-NODIA, No Dialog for this subprogram'
           ELSE
#ifdef  MSP
             CALL FEXSUB ( LOCDIA )
#endif
#ifdef  UNIX
             CALL FEXSUB ( %VAL(LOCDIA) )
#endif
             DONE( ILOC )   = .TRUE.
             LENTMP = LENRD ( NAMCOM(ILOC) )
             CTEMP(LANS(1)) = NAMCOM(ILOC)(1:LENTMP)//'*'
           END IF
         END IF
         IF ( ICOMER .NE. 1 ) RETURN
       IF( NLOOP .LE. 2*MAXQST) GO TO 20
C
       PRINT *,'%Fatal error in FDFANL '
       PRINT *,' Possible infinite loop at FDFANL.',
     > ' Unable to leave FDFANL.'
       STOP
       END
