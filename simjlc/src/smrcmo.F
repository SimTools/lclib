C***********************************************************************
C*
C*==========================================----
C* Subroutine SMRCMO(LNxHLX, HLXVTX, HLXCDC, IRET)
C*==================================------------
C*
C* (Purpose)
C*    Move pivot of CDC Helix parameter to the pivot of VTX 
C*    helix parameter, considering the effect of multiple 
C*    scattering.  This routine is to make a combined 
C*    CDC_VTX track parameter
C* (Inputs)
C*   LNxHLX    : Length of Helix parameter
C*   HLXVTX(*) : VTX Track parameter
C*   HLXCDC(*) : CDC Track parameter
C* (Output)
C*   HLXCDC(*) : CDC track parameter ( pivot is moved)
C*   IRET      : =-1 when failed to move pivot.  
C*   IRET      : =-2 when CDC track does not reach VTX inner most layer.
C*   IRET      : =-3 when inconsistency in the calculation of intersection
C*              If IRET is not zero, CDC helix parameter is not changed.
C* (Relation)
C*   Called by SMRCVT
C* (Updaterecord)
C*     4/06/99  A.Miyamoto  Firs version is created
C*     4/08/99  A.Miyamoto  Skip when particle does not reach VTX layer
C*                          Fix a bug in finding the intersection of
C*                          of low momentum track.
C*
C***********************************************************************
 
      SUBROUTINE SMRCMO(LNxHLX, HLXVTX, HLXCDC, IRET)
 
      IMPLICIT REAL*8 (A-H, O-Z)

#include "prfeld.inc"
#include "unumcn.inc"
#include "smrvgo.inc"
      REAL*4     HLXCDC(LNxHLX), HLXVTX(LNxHLX)
      REAL*4     PIVOT(3,0:MXxVXL)
      REAL*8 CDCT(100)
      REAL*4     HELOUT(100)
      REAL*8     HELERR(50)
      EQUIVALENCE (HELOUT(9),HELERR(1))
C
C========< Entry Point >================================================
C
C--
C  Reset return flag.
C--
      IRET = -1
C--
C  Prepare new pivot data.
C  CDC track is extrapolated from CDC inner layer to the 
C  second VTX measure points, which is the layer outer 
C  VTX pivot.
C--
      RPVVTX=SQRT(HLXVTX(6)**2+HLXVTX(7)**2)
      NLAST=NSMPVX
      NFIRST=0
      DO 100 I = NLAST, 1, -1
         IF( ABS(RPVVTX-RCYVX(I)).GT.0.1 ) GOTO 100
         NFIRST=I
 100  CONTINUE
      NFIRST=NFIRST+1
      IF( NFIRST .LE. 1 ) THEN
         print *,'Error in SMRCMO .. no matching layer was found'
         PRINT *,' RPVVTX=',RPVVTX
         PRINT *,' RCYVX=',(RCYVX(K),K=0,NSMPVX)
         IRET = -1
         RETURN
      ENDIF
C
C---
C  Find the intersects of CDC track with VTX matterial layers
C---
      DO 210 I = 1, 8
         CDCT(I)=HLXCDC(I)
 210  CONTINUE
      CTR=PTOR/CDCT(3)
c      CTRA=ABS(CTR)
      CTXC=CDCT(6)+(CDCT(1)+CTR)*DCOS(CDCT(2))
      CTYC=CDCT(7)+(CDCT(1)+CTR)*DSIN(CDCT(2))
      CTCR=DSQRT(CTXC**2+CTYC**2)
      CVX = HLXVTX(6)
      CVY = HLXVTX(7)
      DIFY = (CVX*CTYC - CVY*CTXC)
      DIFX = (CVX*CTXC + CVY*CTYC)
      DO 200 il=nfirst, nlast
         CVR=RCYVX(IL)
         COSTH=( (CTCR+CTR)*(CTCR-CTR) + CVR*CVR) 
     .           / ( 2*CTCR*CVR ) 

         IF( ABS(COSTH) .GT. 1.0 ) THEN
            IRET=-2
            RETURN
         ENDIF

         THX  = ACOS(COSTH)
C... Sign of THX is ambiguous.  Take the one in the direction
C... same as VTX track parameter
         DIF1 = ATAN2(DIFY+DIFX*TAN(THX),DIFX-DIFY*TAN(THX))
         DIF2 = ATAN2(DIFY-DIFX*TAN(THX),DIFX+DIFY*TAN(THX))
         IF( ABS(DIF2) < ABS(DIF1) ) THX = -THX

         THPIV=ATAN2( CTXC*TAN(THX)+CTYC, 
     >                CTXC - CTYC*TAN(THX))

         XPVOT=CVR*COS(THPIV)
         YPVOT=CVR*SIN(THPIV)

         CH=SIGN(1.0, HLXCDC(3))
         PHIA=CDCT(2)+(1.0+CH)*xPID/2.0

C... Deflaection angle is in the range, -90 to 90 degree
         DEFA=DATAN((-YPVOT+CTYC -(-XPVOT+CTXC)*TAN(PHIA))/
     >             (-XPVOT+CTXC +(-YPVOT+CTYC)*TAN(PHIA)))
         IF( DEFA*CDCT(3) < 0.0) THEN
            PRINT *,'Error in SMRCMO .. Deflection angle',
     >       ' and track charge is inconsistent.'
            PRINT *,' Layer=',IL,' DEF-Angle=',DEFA,
     >               ' Kappa=',CDCT(3)
            IRET=-3
            RETURN
          ENDIF

          ZPVOT=CDCT(8)+CDCT(4)-CTR*CDCT(5)*DEFA
          PIVOT(1,IL)=XPVOT
          PIVOT(2,IL)=YPVOT
          PIVOT(3,IL)=ZPVOT
          
 200  CONTINUE

C ---
C  Move CDC track to VTX layers, then re-evaluate error matrix
C  with Multiple scattering.
C ---

      PCDC=SQRT(1.0+CDCT(5)**2)/ABS(CDCT(3))
C      BETA=1.0
      DO 300 IL = NLAST, NFIRST, -1
         CALL UTRKMO(LNxHLX, HLXCDC, PIVOT(1,IL), HELOUT)
         SIGMS=0.0141/PCDC/SQRT(RDLVX(IL))*
     >         (1.0+ALOG10(RDLVX(IL))/9.0)
         SIGMSQ=SIGMS*SIGMS
C .. E(2,2)=EDAT(3), E(3,3)=Edat(6), E(3,5)=Edat(13), E(5,5)=Edat(15)
         HELERR(3)=HELERR(3)+SIGMSQ*(1.0+HELOUT(5)**2)
         HELERR(6)=HELERR(6)+SIGMSQ*(HELOUT(3)*HELOUT(5))**2
         HELERR(13)=HELERR(13)+SIGMSQ*HELOUT(3)*HELOUT(5)*
     >              (1.0+HELOUT(5)**2)
         HELERR(15)=HELERR(15)+SIGMSQ*(1.0+HELOUT(5)**2)**2
         DO 310 I = 1, LNxHLX
            HLXCDC(I)=HELOUT(I)
 310     CONTINUE
 300  CONTINUE

      RETURN
      END

