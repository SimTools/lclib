C********************************************************************
C*
C* -----------------------------------------
C* Subroutine PRDBNK(MODE, BNKNAM, DSBNAM )
C* -----------------------------------------
C*
C*(Function)
C*   Stack bank and DSB names to be deleted at the end of event.
C*   Same function as SSDBNK.
C*
C*(Input)
C*   MODE .. Mode selection.
C*        = -1  to stack bank names to be output.
C*        >  0  for DSB ID, on which banks are deleted.
C*
C*(Author)
C*   A. Miyamoto  3 Apr. 1987  Created from SSOBNK
C*   A. Miyamoto 13 Feb. 1990  Copied from SSDBNK
C*
C**********************************************************************
C*
      SUBROUTINE PRDBNK(MODE, BNKNAM, DSBNAM)
C*
      CHARACTER*(*) BNKNAM, DSBNAM
      PARAMETER   (MXxSTK=50)
      CHARACTER*64 BNKSTK(MXxSTK), WRK, WRKDSB
      INTEGER*4    IDDSB(MXxSTK), LNBSTK(MXxSTK)
      CHARACTER*64 BNKLST(MXxSTK)
      CHARACTER*130 MESAGE
      COMMON /SSWORK/ BNKLST, MESAGE, WRK, WRKDSB
C
      DATA         NOSTK/ 0 /
C
C=========< Entry Point >==============================================
C
C ----  if MODE <  0 stack required bank name.
C
      IF(MODE.EQ.-2) GO TO 700
      IF(MODE.GE.0) GO TO 500
 
      CALL TBQREC(DSBNAM, IRET)
      IF(IRET.LT.0) THEN
        LWRK   = LEN(DSBNAM)
        WRK    = DSBNAM
        MESAGE = ' DSB '//WRK(:LWRK)//' does not allocated.'
        CALL UERROR(1,DUMMY, 3, 'PRDBNK', MESAGE)
        GO TO 900
      ENDIF
      IF(NOSTK.GE.MXxSTK) THEN
        CALL UERROR(1,DUMMY, 3, 'PRDBNK',
     >           'Too many bank names are defined as to be deleted.')
        GO TO 900
      ENDIF
      NOSTK = NOSTK + 1
      IDDSB(NOSTK)  = IRET
      LBNM = LEN(BNKNAM)
      CALL UCSTRP(BNKNAM(:LBNM),' ',BNKSTK(NOSTK),LNBSTK(NOSTK))
      RETURN
C
C  Mode = 1 case, delete banks in the stack.
C
500   CONTINUE
      IF(NOSTK.LE.0) GO TO 900
      CALL TBQBNK(MODE, WRKDSB, NOBNK, BNKLST)
      IF(NOBNK.LE.0) GO TO 900
 
      DO 600 I = 1, NOBNK
        DO 620 J = 1, NOSTK
          IF( MODE .NE. IDDSB(J) ) GO TO 620
          IF( BNKLST(I)(:LNBSTK(J)).NE.BNKSTK(J)(:LNBSTK(J))) GOTO 620
          CALL TBDELB(MODE, BNKLST(I), IRET)
          GO TO 630
620     CONTINUE
630     CONTINUE
600   CONTINUE
 
      RETURN
C
C ----------------------------------------------------------------------
C (7) Mode = -2, List bank names in the stack.
C ----------------------------------------------------------------------
C
700   CONTINUE
      IF( NOSTK .LE. 0 ) THEN
        WRITE(6,705)
705     FORMAT(5X,'No Banks are deleted from Output.')
        GO TO 900
      ENDIF
      WRITE(6,710)
710   FORMAT(5X,'Following banks are deleted from output.')
      IODSB = 0
      DO 720 I = 1, NOSTK
        IF( IODSB .NE. IDDSB(I) ) THEN
          IODSB = IDDSB(I)
          WRITE(6,730) IODSB, BNKSTK(I)(:LNBSTK(I))
730       FORMAT(10X,' Record ID=',I3,' Bank = ',A,'*')
        ELSE
          WRITE(6,740) BNKSTK(I)(:LNBSTK(I))
740       FORMAT(10X,'               Bank = ',A,'*')
        ENDIF
720   CONTINUE
C
C
900   CONTINUE
      RETURN
      END
