      SUBROUTINE PACHEP(KTO,NPNT)

      INTEGER*4  KTO, NPNT
C
#include "hepevt.inc"
C
C====================< Entry Point >====================================
C
C--
C  Shift /HEPEVT/ entries.
C--
      IOFF   = NPNT - KTO - 2
      IF ( IOFF.EQ.0 ) THEN
         RETURN
      ELSE IF ( IOFF.LT.0 ) THEN
         IPN1ST = KTO + 2
         IPNLST = NHEP
         IPNSTP = +1
      ELSE      
         IPN1ST = NHEP
         IPNLST = KTO + 2
         IPNSTP = -1
      ENDIF
C--
      DO 100 IHEP = IPN1ST, IPNLST, IPNSTP
         ISTHEP(  IHEP+IOFF) = ISTHEP(IHEP)
         IDHEP (  IHEP+IOFF) = IDHEP (IHEP)
         IF ( JMOHEP(1,IHEP).EQ.0 ) THEN
            JMOHEP(1,IHEP+IOFF) = 0
         ELSE
            JMOHEP(1,IHEP+IOFF) = JMOHEP(1,IHEP)+IOFF
         ENDIF
         IF ( JMOHEP(2,IHEP).EQ.0 ) THEN
            JMOHEP(2,IHEP+IOFF) = 0
         ELSE
            JMOHEP(2,IHEP+IOFF) = JMOHEP(2,IHEP)+IOFF
         ENDIF
         IF ( JDAHEP(1,IHEP).EQ.0 ) THEN
            JDAHEP(1,IHEP+IOFF) = 0
            JDAHEP(2,IHEP+IOFF) = 0
         ELSE
            JDAHEP(1,IHEP+IOFF) = JDAHEP(1,IHEP)+IOFF
            JDAHEP(2,IHEP+IOFF) = JDAHEP(2,IHEP)+IOFF
         ENDIF
         DO 10 I = 1, 4
            PHEP(I,IHEP+IOFF) = PHEP(I,IHEP)
            VHEP(I,IHEP+IOFF) = VHEP(I,IHEP)
10       CONTINUE
         PHEP(5,IHEP+IOFF) = PHEP(5,IHEP)
100   CONTINUE
      NHEP = NHEP + IOFF                  
C--
C  That's it.
C--
      RETURN
      END
