CC********************************************************************CC
C*                                                                    *C
C*=========================================----------=====            *C
C*  Subroutine UCYLX(XSTRT,E,XCEN,AXIS,RCYL,XINT,IRET)                *C
C*=========================================----------=====            *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Calculates intersection of a line and a cylinder.               *C
C* (Inputs)                                                           *C
C*      XSTRT  ; starting point of the line.                          *C
C*      E      ; starting direction vector.                           *C
C*               which is not necessarily a unit vector.              *C
C*      XCEN   ; reference point on the axis.                         *C
C*      AXIS   ; axis vector of the cylinder.                         *C
C*      RCYL   ; radius of the cylinder.                              *C
C* (Outputs)                                                          *C
C*      XINT   ; (x,y,z) of 2 intersection points.                    *C
C*               The first corresponds to smaller TEE which           *C
C*               means a particle moving along the line in            *C
C*               the direction of E hits this point first and         *C
C*               then hits the second one.                            *C
C*      IRET   ; = 0 if 2 intersections found.                        *C
C*               = 1 if XSTRT in the cylinder.                        *C
C*               = 2 if XSTRT out of the cylinder and                 *C
C*                      E points outward though the                   *C
C*                      line does have intersections.                 *C
C*               = 3 otherwise.                                       *C
C* (Relation)                                                         *C
C*    Calls UADD3, USUB3, USCLM3, UDOT3, UDOT2, USETRF, UBTRAN        *C
C* (Update Record)                                                    *C
C*   6/09/85  K.Fujii            Original version.                    *C
C*   9/17/87  K.Fujii            Fixed a bug.                         *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UCYLX(XSTRT,E,XCEN,AXIS,RCYL,XINT,IRET)
   
      DIMENSION XSTRT (3), E (3), XCEN (3), AXIS(3)  , XINT (3,2),
     .          XSTRTP(3), EP(3), XCENP(3), EREF(3,3), XINTP(3,2),
     .          XSTCP (3)
C  
C========< Entry Point >================================================
C  
C--
C  Set reference frame which has z-axis in the direction
C  of AXIS vector.
C--
      CALL USETRF(AXIS,EREF)
C--
C  Calculate components in EREF.
C--
      DO 10 I=1,3
         XSTRTP(I) = UDOT3(XSTRT(1),EREF(1,I))
         EP    (I) = UDOT3(E    (1),EREF(1,I))
         XCENP (I) = UDOT3(XCEN (1),EREF(1,I))
10    CONTINUE
C--
C  Calculate the intersection in EREF.
C--
      CALL USUB3(XSTRTP,XCENP,XSTCP)
      A = UDOT2(EP   ,EP   )
      B = UDOT2(EP   ,XSTCP)
      C = UDOT2(XSTCP,XSTCP)-RCYL*RCYL
      D = B*B-A*C
C--
C  Return if no intersection there.
C--
      IF ( D.LT.0.0 .OR. A.EQ.0.0 )              GO TO 9999
      ROOTD  = SQRT(D)
      TEE1  = (-B-ROOTD)/A
      TEE2  = (-B+ROOTD)/A
C--
C  Set IRET.
C--
      IF ( TEE1.GT.0.0 .AND. TEE2.GT.0.0 ) THEN
         IRET = 0
      ELSE IF ( TEE1*TEE2    .LE.         0.0 ) THEN
         IRET = 1
      ELSE
         IRET = 2
      ENDIF
C--
C  Get intersection vectors in EREF.
C--
      CALL USCLM3(TEE1,EP,XINTP(1,1))
      CALL USCLM3(TEE2,EP,XINTP(1,2))
      CALL UADD3(XINTP(1,1),XSTRTP(1),XINTP(1,1))
      CALL UADD3(XINTP(1,2),XSTRTP(1),XINTP(1,2))
C--
C  Rotate them back to the original frame.
C--
      CALL UBTRAN(XINTP(1,1),EREF(1,1),XINT(1,1))
      CALL UBTRAN(XINTP(1,2),EREF(1,1),XINT(1,2))
C--
C  Found intersections.
C--
      RETURN
C--
C  No intersection.
C--
9999  IRET = 3
      RETURN
      END
