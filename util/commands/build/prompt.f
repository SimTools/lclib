C*
C**********************************************************************
C*
C* -------------------------------------
C*  Subroutine PROMPT(N, STRING, IRET)
C* -------------------------------------
C*
C*(Purpose)
C*  Compatibility routine for the subroutine PROMPT in the
C*  FACOM MSP's FORTRAN library.
C*(Input)
C*  N =1 :  Print STRING including <cr>
C*    =2 :  PRINT STRING without <cr>
C*  STRING : Prompt string.
C*(Output)
C*  IRET ; Allways =0
C*(Author)
C*  A. Miyamoto 14-Apr-1994
C*
C**********************************************************************
C*
      SUBROUTINE PROMPT(N, STRING, IRET)
C
      CHARACTER*(*) STRING
      CHARACTER*100 TEMP
C
      IF( N.EQ.1 ) THEN
         WRITE(6,*) STRING
      ELSE
         LSTR=LEN(STRING)
         TEMP='('''//STRING(:LSTR)//''',$)'
         LT = INDEX(' ',TEMP)-1
         WRITE(6,FMT=TEMP) 
      ENDIF
      IRET=0
      RETURN
      END
