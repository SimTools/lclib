C**********************************************************************
C*
C* -----------------
C* Subroutine MAHELP
C* -----------------
C*
C*(Function)
C*   Print Help information.
C*
C*(Author)
C*
C*  A. Miyamoto.   7-Jan-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MAHELP
C
#include "macntl.inc"
C
C------------< Entry Point >-----------------------------------------
C
C (1) Allocate output file.
C
      CALL UERASE
C
      WRITE(MCxMPT,100)
100   FORMAT(
     >' Following commands are available.',/,
     >'   LIST   ... List information of last 10 run.',/,
     >'   PRINT  ... Write run information to the file @.RUNLIST',/,
     >'   END    ... END')
      WRITE(MCxMPT, 110)
110   FORMAT('  ',/,
     >' Example.',/,
     >'  LIST RUN(112)     ... List information of Run#112',/,
     >'  LIST RUN(110:120) ... List information from Run#110 to 120',/,
     >'  LIST DSN          ... List formatted tape volser and DSN',/,
     >'  LIST RUN(111:) DSN .. List formatted tape volser and DSN',
     > ' of run from 111',/,
     >'                       to the last run.',/,
     >'  LIST RUN(111:) EXP(2) ',/,
     >'                    ... List Run information of Exp#2, run from',
     >' 111 to the last',/,
     >'                       If EXP is not given, assumes latest ',
     >'experiment #.',/,
     >'  PRINT command operand is same as LIST command .')
 
      RETURN
      END
