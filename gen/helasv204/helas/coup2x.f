      SUBROUTINE COUP2X(SW2 , GAL,GAU,GAD,GWF,GZN,GZL,GZU,GZD,G1)
C
C This subroutine sets up the coupling constants for the fermion-       
C fermion-vector vertices in the STANDARD MODEL.  The array of the      
C couplings specifies the chirality of the flowing-IN fermion.  G??(1)  
C denotes a left-handed coupling, and G??(2) a right-handed coupling.   
C                                                                       
C INPUT:                                                                
C       real    SW2            : square of sine of the weak angle       
C                                                                       
C OUTPUT:                                                               
C       real    GAL(2)         : coupling with A of charged leptons     
C       real    GAU(2)         : coupling with A of up-type quarks      
C       real    GAD(2)         : coupling with A of down-type quarks    
C       real    GWF(2)         : coupling with W-,W+ of fermions        
C       real    GZN(2)         : coupling with Z of neutrinos           
C       real    GZL(2)         : coupling with Z of charged leptons     
C       real    GZU(2)         : coupling with Z of up-type quarks      
C       real    GZD(2)         : coupling with Z of down-type quarks    
C       real    G1(2)          : unit coupling of fermions              
C
      REAL GAL(2),GAU(2),GAD(2),GWF(2),GZN(2),GZL(2),GZU(2),GZD(2),
     &     G1(2),SW2,ALPHA,FOURPI,EE,SW,CW,EZ,EY
C
      ALPHA=1./128.
C      ALPHA=1./REAL(137.0359895)
      FOURPI=REAL(4.*3.14159265358979323846D0)
      EE=SQRT(ALPHA*FOURPI)
      SW=SQRT(SW2)
      CW=SQRT(1.-SW2)
      EZ=EE/(SW*CW)
      EY=EE*(SW/CW)
C
      GAL(1) =  EE
      GAL(2) =  EE
      GAU(1) = -EE*2./3.
      GAU(2) = -EE*2./3.
      GAD(1) =  EE   /3.
      GAD(2) =  EE   /3.
      GWF(1) = -EE/SQRT(2.*SW2)
      GWF(2) =  0.
      GZN(1) = -EZ*  0.5
      GZN(2) =  0.
      GZL(1) = -EZ*(-0.5+SW2)
      GZL(2) = -EY
      GZU(1) = -EZ*( 0.5-SW2*2./3.)
      GZU(2) =  EY*          2./3.
      GZD(1) = -EZ*(-0.5+SW2   /3.)
      GZD(2) = -EY             /3.
      G1(1)  =  1.
      G1(2)  =  1.
C
      RETURN
      END
