C   03/07/85 704271920  MEMBER NAME  TVSLCT   (FORT)     M  FORTRAN
C.....  Select a graphic device mnemonic
C
        SUBROUTINE TVSLCT(MNEM,NSEL,RESP)
C
        CHARACTER*(*) MNEM(NSEL), RESP
        CHARACTER*50 QUEST /'Graphic device ('/
C
C VAR
        CHARACTER * 8  DEVICE
        DATA DEVICE / '3' /
        SAVE DEVICE
C
C BEGIN
        L = INDEX(QUEST,'(')
        DO 10 I = 1,NSEL
          M = INDEX(MNEM(I)     ,' ')
          IF(M.EQ.0) M=LEN(MNEM(I))+1
          QUEST(L+1:L+M) = MNEM(I)
          L = L+M
          QUEST(L:L+1) = ', '
          L = L+1
10      CONTINUE
        QUEST(L-1:50) = ')'
C       >>> 27-Apr-87, A.Shirahashi, Univ. of Tokyo
C%%     CALL G$COMD(RESP,QUEST(1:L-1))
        CALL TXTRD( QUEST(1:L+1),DEVICE )
        RESP = DEVICE
C       >>> END
        RETURN
        END
C
        SUBROUTINE G$COMD( ANS, PRMPT )
C
        CHARACTER*(*) ANS, PRMPT
C
C       WRITE(6,'(1H ,A)') PRMPT
        CALL PROMPT(2,PRMPT,IERR)
        READ(5,'(A)') ANS
        RETURN
        END
