C***********************************************************************
C*                                                                     *
C*  -----------------------------------------====                      *
C*  Subroutine ULGHED(LU, NRET)
C*  -----------------------------------------====                      *
C*                                                                     *
C*(Function)                                                           *
C*   List Generator:Header bank
C*                                                                     *
C*(Input)                                                              *
C*  LU   ; Logical unit number to print out.
C*(Output)                                                             *
C*  NRET  ; Return code.                                               *
C*                                                                     *
C*(Author)                                                             *
C*  A.Miyamoto  20-Sep-1994  Original version.                         *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE ULGHED( LU , NRET)
C
      COMMON /SSWORK/ IHEAD(20)
C
C ==== < Entry Point >==================================================
C
C
      NRET = -1
      CALL TBGET(1,'Generator:Header', 1, NW, IHEAD, IRET)
      IF( IRET .LT. 0 ) RETURN
      WRITE(LU, 110) IHEAD(1),IHEAD(2), IHEAD(3)
 110  FORMAT(' ************ Generator:Header bank *****',/,
     > ' Event:',I8,'  Date:',I8,' Time:',I13)
      WRITE(LU, 120) IHEAD(4), IHEAD(5), IHEAD(7) 
 120  FORMAT(10X,' Generator ID=',I5,' Seed=',I13,' Ebeam=',I8,' MeV')
      NRET = 0
      RETURN
      END

