CC********************************************************************CC
C*                                                                    *C
C*===============================----===                              *C
C*  Subroutine UP2HLX(XV,PV,ICHG,HELX)                                *C
C*===============================----===                              *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Convert XV, PV to standard helix parametrization.               *C
C*    deflection angle.                                               *C
C* (Inputs)                                                           *C
C*      XV  (*)     : (R*4) ; positon vector.                         *C
C*      PV  (*)     : (R*4) ; momentum vector.                        *C
C*      ICHG        : (I*4) ; charge ( must be +/- 1 ).               *C
C* (Outputs)                                                          *C
C*      HELX(*)     : (R*4) ; helix parameters.                       *C
C*                            1-st 8 words in                         *C
C*                            Production:TPC;Track_Parameter.         *C
C* (Relation)                                                         *C
C*    Calls no subroutines.                                           *C
C* (Update Record)                                                    *C
C*    5/31/88  K.Fujii        Original version.                       *C
C*    4/08/99  A.Miyamoto     Avoid 0 Pt.                             *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UP2HLX(XV,PV,ICHG,HELX)
   
      INTEGER*4   ICHG
      REAL   *4   XV(3), PV(3), HELX(8)
C  
C========< Entry Point >================================================
C  
C--
C  Calculate FI0, CPA, TNL assuming XV as the pivot.
C--
      PX      = PV(1)
      PY      = PV(2)
      PZ      = PV(3)
      PT      = SQRT(PX*PX+PY*PY)
C--
      HELX(1) = 0
      HELX(2) = ATAN2(-PX,PY)
      HELX(4) = 0
      IF( PT .GE. 1.E-9 ) THEN
        HELX(3) = ICHG/PT
        HELX(5) = PZ/PT
      ELSE
        HELX(3) = ICHG*1.E9
        HELX(5) = PZ*1.E9
      ENDIF

      CALL UVCOPY(3,XV,HELX(6))
C--
C  That's it.
C--
      RETURN
      END
