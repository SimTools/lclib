C***********************************************************************
C* 
C*  ------------------=======
C*  Subroutine BGMCLS( MSGCLS )
C*  ------------------=======
C* 
C*(Function)
C*   Returns default message class for BAT command user.
C* 
C*(Input)
C*   none.
C* 
C*(Output)
C*   MSGCLS : Default MSGCLASS of TSS session, CHARACTER*1 variable.
C* 
C*(Author)
C*   A. Miyamoto  11-Oct-1985
C* 
C***********************************************************************
C  
      SUBROUTINE BGMCLS( MSGCLS )
C  
      CHARACTER*8    TRMID
      CHARACTER*1    MSGCLS
      PARAMETER     (NTERM = 12)
C  
      CHARACTER*8    TRMNAM(NTERM)
      CHARACTER*1    DEFCLS(NTERM)
      DATA          (TRMNAM(I), DEFCLS(I),I=1, NTERM)
     > /'TOPAZxxx', 'C',     'TSUKUxxx', 'T',
     >  'LABxxx',   'P',     'ALUxxx',   'C',
     >  'MINAMxxx', 'S',     'AMYxxx',   'C',
     >  'OOHOxxx',  'A',     'SYSxxx',   'C',
     >  'VENUSxxx', 'C',     'FUJIxxx',  'V',
     >  'NIKKOxxx', 'C',     'FACOMxxx', 'C'/
      CHARACTER*1    UNDEF
      DATA           UNDEF/'C'/
C  
C=======< Entrt Point >================================================
C  
      CALL TERMID(TRMID)
      DO 100 I = 1, NTERM
         IF(TRMID(1:2).EQ.TRMNAM(I)(1:2)) GO TO 200
100   CONTINUE
      MSGCLS = UNDEF
      RETURN
200   CONTINUE
      MSGCLS = DEFCLS(I)
      RETURN
      END
