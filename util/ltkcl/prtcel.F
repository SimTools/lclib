      SUBROUTINE PRTCEL
 
#include "prjunk.inc"
      CHARACTER*1 ITST(0:MXxZET,0:MXxPHI)
      CHARACTER*1 SYMB(0:39)
      DATA SYMB / '0','1','2','3','4','5','6','7','8','9',
     .            'A','B','C','D','E','F','G','H','I','J',
     .            'K','L','M','N','O','P','Q','R','S','T',
     .            'U','V','W','X','Y','Z','*','*','*','*' /
      DATA NEVT /0/
 
      WRITE(6,'(1H1)')
      NEVT = NEVT + 1
      PRINT *, ' --------- NEVT = ', NEVT, ' --------- '
      PRINT *, '  '
      PRINT *, '   Generated Hit Cells (phi,zeta)'
      PRINT *, '   Energy is in units of 200 MeV'
      PRINT *, '  '
      PRINT *, '   NCELLS = ', NCELLS
      PRINT *, '  '
 
      DO 100 IX = 0, MXxPHI
         DO 10 IZ = 0, MXxZET
            JBN = JBNPNT(IZ,IX)
            IF ( JBN.GT.0 ) THEN
               ICL = NCLIN(JBN)
               IPH = ICLPNT(1,ICL)/200
               ITST(IZ,IX) = SYMB(MIN(IPH,39))
            ELSE
               ITST(IZ,IX) = '.'
            ENDIF
10       CONTINUE
100   CONTINUE
 
      WRITE(6,'(2X,78A1)') ((ITST(IZ,IX),IX=0,MXxPHI),IZ=0,MXxZET)
 
      WRITE(6,'(1H1)')
      PRINT *, '   # charged tracks in a cell '
      PRINT *, '  '
      PRINT *, '   NCDCTK = ', NCDCTK
      PRINT *, '  '
 
      DO 200 IX = 0, MXxPHI
         DO 20 IZ = 0, MXxZET
            KBN = KBNPNT(IZ,IX)
            IF ( KBN.GT.0 ) THEN
               NTK = NTKIN(KBN)
               ITST(IZ,IX) = SYMB(NTK)
            ELSE
               ITST(IZ,IX) = '.'
            ENDIF
20       CONTINUE
200   CONTINUE
 
      WRITE(6,'(2X,78A1)') ((ITST(IZ,IX),IX=0,MXxPHI),IZ=0,MXxZET)
 
      RETURN
      END
