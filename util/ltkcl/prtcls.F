      SUBROUTINE PRTCLS(MODE)
 
#include "prjunk.inc"
#include "smrhgo.inc"
      CHARACTER*1 ITST(0:MXxZET,0:MXxPHI)
      CHARACTER*1 SYMB(40)
      DATA SYMB / '1','2','3','4','5','6','7','8','9','A',
     .            'B','C','D','E','F','G','H','I','J','K',
     .            'L','M','N','O','P','Q','R','S','T','U',
     .            'V','W','X','Y','Z','*','*','*','*','*' /
 
      IF ( MODE.EQ.1 ) THEN
         NCLST = 0
         DO 100 IX = 0, MXxPHI
            DO 10 IZ = 0, MXxZET
               ITST(IZ,IX) = '.'
10          CONTINUE
100      CONTINUE
      ELSE IF ( MODE.EQ.2 ) THEN
         IF ( IWORK(1).GT.0 ) THEN
            NCLST = NCLST + 1
#ifdef CP
          PRINT *, ' --- IWORK dump ----'
#endif 
         ENDIF
         DO 200 JFI = 2, 2*IWORK(1), 2
            IPT  = IWORK(JFI+1)
            NCL  = IWORK(IPT)
C>>>
#ifdef CP
           PRINT *, ' JFI, IPT, NCEL = ', JFI, IPT, NCL
#endif 
C>>>
            DO 20 ICL = 1, NCL
C>>>
#ifdef CP
           PRINT *, ' IWORK(IPT+1) = ', IWORK(IPT+1)
#endif 
#ifdef CP
           PRINT *, '      (IPT+2) = ', IWORK(IPT+2)
#endif 
#ifdef CP
           PRINT *, '      (IPT+3) = ', IWORK(IPT+3)
#endif 
C>>>
               IDCEL = IWORK(IPT+2)
               IDCLA = IABS(IDCEL)
               IZE   = IDCEL/1000000
               ITH   = MOD(IDCLA/1000,1000)
               IFI   = MOD(IDCLA,1000)
               IF ( IZE.EQ.-1 ) THEN
                  IZT   = (NRADHD-ITH-1) + NTHTHD + NRADHD
               ELSE IF ( IZE.EQ.0 ) THEN
                  IZT   = ITH + NRADHD
               ELSE
                  IZT   = ITH
               ENDIF
               ITST(IZT,IFI) = SYMB(MIN(NCLST,40))
               IPT = IPT + 3
20          CONTINUE
200      CONTINUE
C>>>
#ifdef CP
        PRINT *, '   '
#endif 
C>>>
      ELSE IF ( MODE.EQ.3 ) THEN
         WRITE(6,'(1H1)')
         PRINT *, '   Reconstructed'
         PRINT *, '  '
         PRINT *, '   NCLST = ', NCLST
         PRINT *, '  '
         WRITE(6,'(2X,78A1)') ((ITST(IZ,IX),IX=0,MXxPHI),IZ=0,MXxZET)
      ENDIF
 
      RETURN
      END
