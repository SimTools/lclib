C***********************************************************************
C*
C*======================================---==
C* Subroutine SMRSHA(ITYP,MXxHSG,DXY,XY,EXY)
C*======================================---==
C*
C* (Purpose)
C*     Calculate shower energy deposit in a rectangular cell.
C* (Inputs)
C*        ITYP     : (I*4) :  (1,2) = (EM,HD).
C*        MXxHSG   : (R*4) :  -MXxHSG-->+MXxHSG.
C*        DXY( 1)  : (R*4) :  X_width.
C*           ( 2)  : (R*4) :  Y_width.
C*        XY ( 1)  : (R*4) :  X.
C*           ( 2)  : (R*4) :  Y.
C* (Output)
C*        EXY(i,j) : (R*4) :  energy fraction in cell (i,j).
C* (Relation)
C*     Called by SMREMC and SMRHDC.
C*     Calls no subroutines.
C* (Updaterecord)
C*     3/29/93  K.Fujii           Original version.
C*
C***********************************************************************
 
      SUBROUTINE SMRSHA(ITYP,MXxHSG,DXY,XY,EXY)
 
      INTEGER*4  ITYP, MXxHSG
      REAL   *4  DXY(2), XY(2), EXY(-MXxHSG:MXxHSG,-MXxHSG:MXxHSG)
      REAL   *4  ANM(2,2), ALM(2,2), PXY(-20:20,2)
      DATA NCALL /0/
C
C========< Entry Point >================================================
C
C--
C  Initialize a lateral shower shape function:
C     f(x) = ANM(1,i)*EXP(-!x!/ALM(1,i)) + ANM(2,i)*EXP(-!x!/ALM(2,i))
C                   core part                 peripheral part
C  where i = (1,2) = (EM,HD).
C--
      IF ( NCALL.EQ.0 ) THEN
         NCALL = 1
         ANM(1,1) = 0.14/1.263/2
         ANM(2,1) = 1.00/1.263/2
         ALM(1,1) = 0.45
         ALM(2,1) = 1.20
         ANM(1,2) = 2.00/11.40/2
         ANM(2,2) = 1.00/11.40/2
         ALM(1,2) = 2.20
         ALM(2,2) = 7.00
      ENDIF
C--
C  Decide calorimeter type.
C--
      IF ( ITYP.NE.1 .AND. ITYP.NE.2 ) THEN
         PRINT *, ' >>>>>> Error in SMRSHW >>>>>>> '
         PRINT *, '     ITYP = ', ITYP, ' is not supported.'
         PRINT *, '     STOP.'
         STOP
      ENDIF
      AL1 = ALM(1,ITYP)
      AL2 = ALM(2,ITYP)
      A1  = AL1*ANM(1,ITYP)
      A2  = AL2*ANM(2,ITYP)
C--
C  Integrate f(x)*f(y) over the cell.
C--
      DO 1000 I = 1, 2
         AX  = ABS(XY(I)+DXY(I)*MXxHSG)
         PRR = A1*(1-EXP(-AX/AL1)) + A2*(1-EXP(-AX/AL2))
         DO 100 IX = -MXxHSG+1, MXxHSG+1
            PRL = PRR
            AX  = ABS(XY(I)-DXY(I)*IX)
            PRR = A1*(1-EXP(-AX/AL1)) + A2*(1-EXP(-AX/AL2))
            IS  = -SIGN(1.1,IX-.1)
            PRR = IS*PRR
            PXY(IX-1,I) = PRL - PRR
100      CONTINUE
1000  CONTINUE
C--
      DO 2000 J = -MXxHSG, MXxHSG
         DO 200 I = -MXxHSG, MXxHSG
               EXY(I,J) = PXY(I,1)*PXY(J,2)
200      CONTINUE
2000  CONTINUE
C--
C  That's it.
C--
      RETURN
      END
