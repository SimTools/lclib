CC********************************************************************CC
C*                                                                    *C
C*=========================================                           *C
C*  Subroutine UBINIT(X,BNDRY,NBIN,IBIN)                              *C
C*=========================================                           *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Decides which bin X falls on when the number of bins and        *C
C*    bin boundary are given by NBIN and BNDRY, respectively.         *C
C* (Arguments)                                                        *C
C*      1) Inputs                                                     *C
C*          X        ; variable to bin into                           *C
C*          BNDRY(I) ; bin boundary                                   *C
C*                      I = 1     ; left limit                        *C
C*                        = NBIN  ; right limit                       *C
C*          NBIN     ; Number of bins + 1                             *C
C*      2) Output                                                     *C
C*          IBIN     ; bin number on which X falls                    *C
C* (Update Record)                                                    *C
C*      11/27/84  KXF  Original version.                              *C
C*                                                                    *C
CC********************************************************************CC
C  
C =========================================
      SUBROUTINE UBINIT(X,BNDRY,NBIN,IBIN)
C =========================================
C  
      DIMENSION BNDRY(NBIN)
C  
      DO 100 I=1,NBIN-1
        IF(X.LT.BNDRY(I).OR.X.GE.BNDRY(I+1)) GO TO 100
        IBIN = I
        GO TO 200
 100  CONTINUE
        IBIN = -999
 200  CONTINUE
C  
      RETURN
      END
