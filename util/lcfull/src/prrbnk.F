C********************************************************************
C*
C* -----------------------------------------
C* Subroutine PRRBNK(MODE, BNKNAM, DSBNAM )
C* -----------------------------------------
C*
C*(Function)
C*   Stack bank and DSB names to be deleted at the begining of
C*  event, and deletes unneccesary banks at the end of event.
C*
C*(Input)
C*   MODE .. Mode selection.
C*        = -1  to stack bank names to be output.
C*        = -2  to List bank and record name stacked.
C*        >  0  for DSB ID, on which banks are deleted.
C*
C*(Author)
C*   A. Miyamoto  April 30, 1987  Modified from SSDBNK
C*
C**********************************************************************
C*
      SUBROUTINE PRRBNK(MODE, BNKNAM, DSBNAM)
C*
      CHARACTER*(*) BNKNAM, DSBNAM
      PARAMETER   (MXxSTK=100)
      CHARACTER*64 BNKSTK(MXxSTK), WRK, WRKDSB
      INTEGER*4    IDDSB(MXxSTK), LNBSTK(MXxSTK)
      CHARACTER*64 BNKLST(MXxSTK)
      COMMON /SSWORK/ BNKLST
C
C .... Standard output banks.
      PARAMETER   (NOUTBK=0)
C
C     BNKSTK and IDDSB is a stack to remember bank names and DSB ID,
C   which is required to output.
C
C
      DATA         NOSTK/ NOUTBK /
C
C---------< Entry Point >------------------------------------------
C
C ----  if MODE <  0 stack required bank name.
C
      IF(MODE.EQ.-2) GO TO 700
      IF(MODE.GE.0) GO TO 500
      CALL TBQREC(DSBNAM, IRET)
      IF(IRET.LT.0) THEN
        LWRK = LEN(DSBNAM)
        WRK = DSBNAM
        CALL UERROR(1,DUMMY, 3, 'PRRBNK',
     >              ' DSB '//WRK(:LWRK)//' does not allocated.')
        GO TO 900
      ENDIF
      IF(NOSTK.GE.MXxSTK) THEN
        CALL UERROR(1,DUMMY, 3, 'PRRBNK',
     >           'Too many bank names are defined as to be deleted.')
        GO TO 900
      ENDIF
      NOSTK = NOSTK + 1
      IDDSB(NOSTK)  = IRET
      LBNM = LEN(BNKNAM)
      CALL UCSTRP(BNKNAM(:LBNM),' ',BNKSTK(NOSTK),LNBSTK(NOSTK))
      RETURN
C
C ----------------------------------------------------------------------
C  Mode = 1 case, delete unnecessary banks,
C ----------------------------------------------------------------------
C
500   CONTINUE
      IF(NOSTK.LE.0) GO TO 900
 
      DO 510 I = 1, NOSTK
        IF(IDDSB(I).EQ.MODE) GO TO 520
510   CONTINUE
      GO TO 900
 
520   CONTINUE
      CALL TBQBNK(MODE, WRKDSB, NOBNK, BNKLST)
      IF(NOBNK.LE.0) GO TO 900
      DO 600 I = 1, NOBNK
        DO 620 J = 1, NOSTK
          IF(MODE.NE.IDDSB(J)) GO TO 620
          IF(BNKLST(I)(:LNBSTK(J)).EQ.BNKSTK(J)(:LNBSTK(J))) THEN
            CALL TBDELB(MODE, BNKLST(I), IRET)
            GO TO 600
          ENDIF
620     CONTINUE
600   CONTINUE
      RETURN
C
C
C ----------------------------------------------------------------------
C (7) Mode = -2, List bank names in the stack.
C ----------------------------------------------------------------------
C
700   CONTINUE
      IF( NOSTK .LE. 0 ) THEN
        WRITE(6,705)
705     FORMAT(5X,'No Banks are removed from input.')
        GO TO 900
      ENDIF
      WRITE(6,710)
710   FORMAT(5X,'Following banks are removed from input.')
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
900   CONTINUE
      RETURN
      END
