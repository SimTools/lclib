C***********************************************************************
C*                                                                     *
C*  -----------------------------------------====                      *
C*  Subroutine GENELS( IDREC, LEVEL, IDEBUG, NRET )                    *
C*  -----------------------------------------====                      *
C*                                                                     *
C*(Function)                                                           *
C*   List event information.                                           *
C*                                                                     *
C*(Input)                                                              *
C*  IDREC ; record ID, use only ID=1                                   *
C*  LEVEL ; not used.                                                  *
C*  IDEBUG; not used.                                                  *
C*(Output)                                                             *
C*  NRET  ; Return code.                                               *
C*                                                                     *
C*(Author)                                                             *
C*  A.Miyamoto  20-Sep-1994  Original version.                         *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE GENELS( IDREC, LEVEL, IDEBUG, NRET )
C
      COMMON /SSWORK/ IHEAD(20), NOELM(500), RBUF(20,500)

      DATA  ILEVT/0/
C
C ==== < Entry Point >==================================================
C
C
      NRET = 0
      IF( IDREC .NE. 1 ) RETURN


C
C  -----------------------------------------------------
C (1) Print Generator:Header, and Generator:Particle_List
C  -----------------------------------------------------
C
      CALL ULGHED(6, IRET)
      IF( IRET .LT. 0 ) RETURN
      CALL ULGPRT(6, IRET)


      RETURN
      END
