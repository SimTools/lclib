C***********************************************************************
CC*********************************************************************C
C*
C*-------------------=======
C* Subroutine PRCSEQ(CMDNAM)
C*-------------------=======
C*
C*(Purpose)
C*    Change default data analysis sequence.
C*
C*(Input)
C*    CMDNAM ; Command name.
C*
C*(Author)
C*    A. Miyamoto   27-Sep-1994  Original version.
C*
CC**********************************************************************
C
      SUBROUTINE PRCSEQ(CMDNAM)
C
#include "fldscr.inc"
C
      CHARACTER*(*) CMDNAM
      CHARACTER     MODNAM*200
      DATA          MODNAM/' '/
      CHARACTER*16  TOKEN(20), WRK
      INTEGER*4     LTOK(20)
C
C=======< Entry Point >=================================================
C
C (1) Define operand for the command 'CONSTANT at the initial call.
C
      LCMD = LEN(CMDNAM)
C
      IF(CMDNAM(1:1).NE.']') GO TO 200
C
      IF(CMDNAM(:LCMD).EQ.']ANAL') THEN
        CALL DBCOPR('ANAL','SEQ',    'C*200',MODNAM,IRET)
      ENDIF
      GO TO 900
C
C ----------------------------------------------------------------------
C (2) Process input command.
C ----------------------------------------------------------------------
C
200   CONTINUE
C

      LM = LNBLNK(MODNAM)
      CALL UCTOKN(MODNAM(:LM), ' ',20, NTOK, LTOK, TOKEN)
      DO 220 I = 1, NTOK
         CALL UCUPER(LTOK(I),TOKEN(I))
 220  CONTINUE

      LISTSP(0) = 0
      DO 210 I = 1, NTOK
         DO 300 JP = 1, NPROG
            WRK = ENTNAM(JP)
            CALL UCUPER(LTOK(I),WRK)
            IF(WRK(:LTOK(I)).EQ.TOKEN(I)(:LTOK(I))) THEN
              LISTSP(0) = LISTSP(0) + 1
              LISTSP(LISTSP(0)) = JP
              GO TO 310
            ENDIF
 300     CONTINUE
 310     CONTINUE
 210  CONTINUE
C
      PRINT *,'PRCRED: ',LISTSP(0),
     > ' analysis modules are defined with a sequence ;'
      WRITE(6,'(3X,5(A,2X))') (ENTNAM(LISTSP(K)),K=1,LISTSP(0))
        

C
C (9) Normal return
C
900   CONTINUE
      RETURN
      END
