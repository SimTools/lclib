C**********************************************************************
C* 
C* ---------------------------------------==================
C* Subroutine UCTOKN( INPUT, SEP, MXxBUF, NTOK, LTOK, TOKEN )
C* ---------------------------------------==================
C* 
C*(Function)
C*   Devide INPUT string by SEP to NTOKs strings.
C* 
C*(Input)
C*   INPUT  : Target string.
C*   SEP    : Separator string.
C*   MXxBUF : Buffer size of TOEKN, TOEKN(MXxBUF)
C* 
C*(Output)
C*   NTOK   : # of separated string.
C*   LTOK   : Length of each toekn.
C*   TOKEN  : Token string.
C* 
C*(Author)
C*   A. Miyamoto    16-Sep-1987  Original version.
C* 
C**********************************************************************
C* 
      SUBROUTINE UCTOKN( INPUT, SEP, MXxBUF, NTOK, LTOK, TOKEN)
C* 
      CHARACTER*(*) INPUT, TOKEN(MXxBUF), SEP
      INTEGER*4     LTOK(MXxBUF)
C  
C =====< ENTRY POINT >=================================================
C  
      LENINP = LEN(INPUT)
      LENTOK = LEN(TOKEN(1))
      LENSEP = LEN(SEP)
C  
      IPS    = 0
      NTOK   = 0
      KSTA   = 1
100   CONTINUE
      IPS    = IPS + 1
      IPL    = IPS + LENSEP - 1
      IF( IPL .GT. LENINP ) GO TO 500
120   CONTINUE
      IF( INPUT(IPS:IPL) .NE. SEP(:LENSEP) ) GO TO 100
      IF( IPS .EQ. 1 ) GO TO 300
C  
C ... Separator was found
C  
      NTOK        = NTOK + 1
      TOKEN(NTOK) = INPUT(KSTA:IPS-1)
      LTOK(NTOK)  = IPS - KSTA
      KSTA        = 0
C  
C ... Search the character not equal to Separator.
C  
300   CONTINUE
      IPS    = IPS + 1
      IPL    = IPS + LENSEP - 1
      IF( IPL .GT. LENINP ) GO TO 700
      IF( INPUT(IPS:IPL) .EQ. SEP(:LENSEP) ) GO TO 300
      KSTA   = IPL
      GO TO 120
C  
C  
500   CONTINUE
      IF( KSTA .NE. 0 ) THEN
        NTOK        = NTOK + 1
        TOKEN(NTOK) = INPUT(KSTA:IPS-1)
        LTOK(NTOK)  = IPS - KSTA
        KSTA        = 0
      ENDIF
C  
700   CONTINUE
      RETURN
      END
