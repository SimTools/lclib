C***********************************************************************
C* 
C* ----------------------------------------------=====
C* Subroutine UCHANG( CFROM, CTO, NLINE, STRING , NRET)
C* ----------------------------------------------=====
C* 
C*(Function)
C*  Change character string CFROM to CTO in the Character variable
C*  STRING.  Corresponds to the CHANGE command of PFDE editor.
C* 
C*(Input)
C*  CFROM  : From string.
C*  CTO    : To string.
C*  NLINE  : # of lines
C*  STRING : Tarcget string array, defined as STRING(NLINE)
C*           Length should be less or equal to 256.
C* 
C*(Output)
C*  STRING : Answer after change.
C*  NRET   : = 0   no CFROM string found in STRING.
C*           > 0   # of CFROM found in STRING.
C*           =-1   When input string is longer than 256.
C* 
C*(Author)
C*  A. Miyamoto   16-May-1987  Original version.
C* 
C***********************************************************************
C* 
      SUBROUTINE UCHANG( CFROM, CTO, NLINE, STRING, NRET)
C  
      CHARACTER*(*) CFROM, CTO
      CHARACTER*(*) STRING(NLINE)
      CHARACTER*256 WSTR
C     CHARACTER*4   CWRK
C     EQUIVALENCE  (CWRK,LCWRK)
C  
C ====< Entry Point >===================================================
C  
C ----------------------------------------------------------------------
C (1) Initialization, get character variable length.
C ----------------------------------------------------------------------
C  
      LFROM = LEN(CFROM)
C     CWRK  = CTO(1:4)
C     IF( LCWRK .EQ. 0 ) THEN
C       LT0   = 0
C     ELSE
        LTO   = LEN(CTO)
C     ENDIF
      LSTR  = LEN(STRING(1))
      NRET  = 0
      IF( LSTR .GT. 256 ) THEN
         NRET = -1
         RETURN
      ENDIF
C  
C ----------------------------------------------------------------------
C (2) Loop over lines to change string.
C ----------------------------------------------------------------------
C  
      DO 200 LINE = 1, NLINE
        ICONV = 0
210     CONTINUE
        ICOL = INDEX(STRING(LINE), CFROM(:LFROM))
        IF(ICOL.LE.0) GO TO 200
C  
C ----------------------------------------------------------------------
C (3) Found string, so convert it.
C ----------------------------------------------------------------------
C  
        IF( LTO .GT. 0 ) THEN
          WSTR(:LSTR)  = STRING(LINE)(:ICOL-1)//CTO(:LTO)//
     >                   STRING(LINE)(ICOL+LFROM:LSTR)
        ELSE
          WSTR(:LSTR)  = STRING(LINE)(:ICOL-1)//
     >                   STRING(LINE)(ICOL+LFROM:LSTR)
        ENDIF
        STRING(LINE) = WSTR(:LSTR)
        NRET         = NRET  + 1
        ICONV        = ICONV + 1
        IF( ICONV .LE. 256 ) GO TO 210
   
        PRINT *,'%Fatal Error in UCHANG .. Infinite Loop.'
        PRINT *,' CFROM  =',CFROM(:LFROM)
        PRINT *,' CTO    =',CTO(:LTO)
        PRINT *,' STRING =',STRING(LINE)(:LSTR)
        CALL ERRTRA
        STOP
   
200   CONTINUE
      RETURN
      END
