CC********************************************************************CC
C*                                                                    *C
C*=============================-----====                              *C
C*  Subroutine UCOLLN(XVV,XV,P,SINTH)                                 *C
C*=============================-----====                              *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    routine to calculate collinearity.                              *C
C* (Input)                                                            *C
C*      XVV(3) = secondary vertex position vector.                    *C
C*      XV (3) = primary vertex position vector.                      *C
C*      P  (3) = momentum of a particle in question.                  *C
C* (Output)                                                           *C
C*      SINTH  = collinearity defined to be SIN(THETA) of direction   *C
C*               of the momentum and direction of the line segment    *C
C*               connecting primary and secondary verteces.           *C
C* (Relation)                                                         *C
C*    Calls                                                           *C
C*       USUB3, UCROSS, UDOT3                                         *C
C* (Update Record)                                                    *C
C*    7/29/85  K. Fujii  Original version.                            *C
C*                                                                    *C
CC********************************************************************CC
C  
C ===============================-----====
      SUBROUTINE UCOLLN(XVV,XV,P,SINTH)
C ===============================-----====
C  
      DIMENSION XV(3),XVV(3),XVXVV(3),P(3),CLLV(3)
      CALL USUB3(XVV,XV,XVXVV)
      CALL UCROSS(XVXVV,P,CLLV)
      SINTH=UDOT3(CLLV,CLLV)
      SINTH=SINTH/UDOT3(XVXVV,XVXVV)
      SINTH=SINTH/UDOT3(P,P)
      SINTH=SQRT(SINTH)
      RETURN
      END
