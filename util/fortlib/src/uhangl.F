CC********************************************************************CC
C*                                                                    *C
C* ==============================-------===                           *C
C*  Subroutine UHANGL(DK,RES,ZED,CTH,PHI)                             *C
C* ==============================-------===                           *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   calculates decay angles of particle DK in RES rest frame.        *C
C*   ZED is ref. Z direction  (Beam for t channel, -MM for            *C
C*   s channel)                                                       *C
C*   If ZED = +/-RES,  (RES helicity frame) decay plane is            *C
C*   undefined & PHI will be zero.                                    *C
C*   DK, RES, and ZED are all 4-vectors.                              *C
C* (Inputs)                                                           *C
C*       DK  : a particle in question.                                *C
C*       RES : in whose rest frame decay angles will be evaluated.    *C
C*       ZED : whose direction of flight defines ref. z-direction.    *C
C* (Outputs)                                                          *C
C*   CTH, PHI: Helicity angles (Jackson-Treiman if IZED is beam)      *C
C* (Relation)                                                         *C
C*   Calls                                                            *C
C*     UBSTFD, UNDOT3, UNRMV3, UNCROS.                                *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii  derived from HANGEL in LASS utility.          *C
C*                                                                    *C
CC********************************************************************CC
C* 
C* =========================================
      SUBROUTINE UHANGL(DK,RES,ZED,CTH,PHI)
C* =========================================
C  
      DIMENSION XAXIS(3),YAXIS(3),ZAXIS(3),
     .          DK(4),RES(4),ZED(4),DKR(4),ZEDR(4)
C----
C  Transform DK to RES rest frame (Store in DKR)
C----
      CALL UBSTFD(DK(1),RES(1),DKR(1))
C----
C  Calculate Y-axis direction
C----
      RESZ = UNDOT3(RES,ZED)
      IF ( 1.-ABS(RESZ).GT.1.0E-6 ) GO TO 10
C  
C --- Y-axis undefined. special treatment for helicity frame.
C  
      CALL UNRMV3(ZED(1),ZAXIS(1))
      PHI=0.
      GO TO 15
C  
   10 CALL UNCROS (ZED(1),RES(1),YAXIS)
C----
C  Transform ZED to RES rest frame to get Z-axis.
C----
      CALL UBSTFD(ZED(1),RES(1),ZEDR(1))
      CALL UNRMV3(ZEDR(1),ZAXIS(1))
C----
C  Calculate X-axis direction
C----
      CALL UNCROS (YAXIS,ZAXIS,XAXIS)
C  
      SC  = UDOT3(XAXIS(1),DKR(1))
      SS  = UDOT3(YAXIS(1),DKR(1))
      PHI = ATAN2(SS,SC)
C  
   15 CTH = UNDOT3(ZAXIS(1),DKR(1))
C  
      RETURN
      END
