CC********************************************************************CC
C*                                                                    *C
C* =================================----------=====                   *C
C*  Subroutine UPLNX(XSTRT,E,XPLN,EN,XINT,IRET)                       *C
C* =================================----------=====                   *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Calculates intersection of a line and a plane.              *C
C*    2) I/O Specifications                                           *C
C*        Inputs                                                      *C
C*          XSTRT  ; starting point of the line.                      *C
C*          E      ; starting direction vector.                       *C
C*          XPLN   ; reference point on the plane.                    *C
C*          EN     ; normal vector to the plane.                      *C
C*        Outputs                                                     *C
C*          XINT   ; intersection point.                              *C
C*          IRET   ; = 0 if intersection found.                       *C
C*                   = 1 otherwise.                                   *C
C*    3) Relation                                                     *C
C*        Invoked by                                                  *C
C*          VRTRIG ; in NCSRAY loop.                                  *C
C*        Calls                                                       *C
C*          UADD3, USUB3, USCLM3, UDOT3                               *C
C*    4) Update record                                                *C
C*         6/09/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C* ===================================----------=====
      SUBROUTINE UPLNX(XSTRT,E,XPLN,EN,XINT,IRET)
C* ===================================----------=====
C  
      DIMENSION XSTRT(3),E(3),XPLN(3),EN(3),XINT(3)
C  
C --- Check if intersection there.
C  
      EDOTEN = UDOT3(E,EN)
      IF ( EDOTEN.EQ.0. ) GO TO 9999
C  
C --- Calculate intersection.
C  
      IRET = 0
C  
      CALL USUB3(XPLN,XSTRT,XINT)
      TEE = UDOT3(EN,XINT)/EDOTEN
C  
      CALL USCLM3(TEE,E,XINT)
      CALL UADD3(XINT,XSTRT,XINT)
C  
C --- Found an intersection
C  
      RETURN
C  
C --- No intersection
C  
 9999 IRET = 1
      RETURN
      END
