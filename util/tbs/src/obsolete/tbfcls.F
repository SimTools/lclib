C**************************************************
C*
C*  Subroutine TBFCLS( NRET )
C*
C*(Function)
C*  Close RMTL unit.
C*(Output)
C*  NRET < 0 When error is detected.
C*(Author)
C*  A. Miyamoto  31-May-1994
C*
C***************************************************
C*
      SUBROUTINE TBFCLS( NRET )
C*
 
      CHARACTER*1  NULL
      DATA NULL/X'00'/
 
      NRET = 0
      IUNIT = 10
      CALL MTLCLS( IUNIT, IRC )
      IF( IRC .NE. 0 ) THEN
         PRINT *,'In TBFCLS ... TBFCLS NRET =',IRC
         NRET = -2
      ENDIF
 
C ************************************************************** *
C     CALL MTLFRE
C ************************************************************** *
 
      CALL MTLFRE( IUNIT,'DD(FT10F001)'//NULL,IRC,IRC1,IRC2 )
C
      WRITE(6,*) 'MTLFRE return code =',IRC
      IF (IRC.NE.0) THEN
        WRITE(6,*) 'IPFCMD RC1=',IRC1
        WRITE(6,*) 'IPFCMD RC2=',IRC2
        NRET = -1
      END IF
C ************************************************************** *
C     CALL MTLEND
C ************************************************************** *
 
 9000 CONTINUE
      CALL MTLEND( IRC )
      WRITE(6,*) 'MTLEND return code =',IRC
 
      RETURN
      END
