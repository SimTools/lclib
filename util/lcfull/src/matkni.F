C**********************************************************************
C*
C* -----------------
C* Subroutine MATKNI(INPUT, ARGMNT, NRET, NANS1, NANS2)
C* -----------------
C*
C*(Function)
C*   Interprete argument string.
C*
C*(Input)
C*   INPUT  ; COmmand string.
C*   ARGMNT ; Operand string.
C*
C*(Output)
C*   NRET   = 0 when operand string is found.
C*          =-1 when not found.
C*   NANS1  = First ans.
C*   NANS2  = Second ans.
C*
C*       exp RUN(n1:n2)
C*
C*(Author)
C*
C*  A. Miyamoto.  12-Jan-1987   Original version.
C*
C**********************************************************************
C*
      SUBROUTINE MATKNI(INPUT, OPERND, NRET, NANS1, NANS2)
C
      CHARACTER*(*) INPUT, OPERND
      CHARACTER*80  WORK, WORK2
C
C============< Entry Point >=========================================
C
      NRET = 0
      LINP = LEN(INPUT)
      LOPR = LEN(OPERND)
C
      IP1  = INDEX(INPUT,OPERND)
      IF(IP1.EQ.0) THEN
        NRET = -1
        RETURN
      ENDIF
C
      IP2 = INDEX(INPUT(IP1+LOPR:LINP), ')')
      IF(IP2.EQ.0) THEN
        NRET = -2
        RETURN
      ENDIF
C
      LWORK = IP2 - 1
      WORK = INPUT(IP1+LOPR:IP1+LOPR+IP2-2)
      CALL UCSTRP(WORK, ' ',WORK2, LWRK2)
      IF(LWRK2.EQ.0) THEN
         NANS1 = 0
         NANS2 = 0
      ELSEIF(WORK2(1:1).EQ.':') THEN
         NANS1 = 0
         READ(WORK2(2:LWRK2),*) NANS2
      ELSEIF(WORK2(LWRK2:LWRK2).EQ.':') THEN
         READ(WORK2(1:LWRK2-1),*) NANS1
         NANS2 = 0
      ELSE
         IP = INDEX(WORK2(:LWRK2), ':')
         IF(IP.EQ.0) THEN
           READ(WORK2(:LWRK2),*) NANS1
           NANS2 = NANS1
         ELSE
           READ(WORK2(:IP-1),*) NANS1
           READ(WORK2(IP+1:LWRK2),*) NANS2
         ENDIF
      ENDIF
C
      RETURN
      END
