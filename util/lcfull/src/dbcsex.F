COPTIONS @OPTIONS  NODEBUG
C***********************************************************************
C*
C* ---------------------------------
C*  Subroutine DBCSEX(CMDNAM, SUBNAM)
C* ---------------------------------
C*
C*(Function)
C*    Execute subroutine assigned to the command.
C*
C*(Input)
C*    CMDNAM   ; command name.
C*    SUBNAM   ; Subroutine address to be executted.
C*
C*(Author)
C*    A. Miyamoto    Jun-16-1986  Original version.
C*
C***********************************************************************
C
      SUBROUTINE DBCSEX(CMDNAM, SUBNAM)
C
      CHARACTER*(*)   CMDNAM
C
C------< Entry Point >--------------------------------------------------
C
C (1) Execute subroutine.
C
      CALL SUBNAM(CMDNAM)
 
      RETURN
      END
