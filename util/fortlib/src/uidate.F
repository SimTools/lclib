C*********************************************************************
C* 
C*    ------------------======
C*    Subroutine UIDATE(IDATE)
C*    ------------------======
C* 
C*(Function)
C*    Converts character format Date (Date obtained by Facom standard
C*   date routine with format yy-mm-dd ) into TOPAZ standard date
C*   format, yymmdd in INTEGER*4 format.
C* 
C*(Output)
C*    IDATE   :  Integer*4 variable of Date with format yymmdd.
C* 
C*(Author)
C*    A. Miyamoto        19-Dec-1985  Original version.
C* 
C*********************************************************************
C* 
      SUBROUTINE UIDATE(IIDATE)
C  
#if defined(G77_521_OR_GREATER)
      INTEGER*4 ITMP(3)
      CALL IDATE(ITMP)
      IY = ITMP(3)
      IM = ITMP(2)
      ID = ITMP(1)
#else
      CHARACTER*8 HIZUKE
      CALL DATE(HIZUKE)
      READ(HIZUKE(1:2),'(I2)') IY
      READ(HIZUKE(4:5),'(I2)') IM
      READ(HIZUKE(7:8),'(I2)') ID
#endif
      IIDATE = ID + 100*IM + 10000*IY
      RETURN
      END
