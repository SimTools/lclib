CC********************************************************************CC
C*                                                                    *C
C*===================---------======                                  *C
C* Subroutine PRTRGT(TYP,TIME,IDEB)                                   *C
C*===================---------======                                  *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*    Unpack "Raw_Data:Trigger" bank.                                 *C
C* (Input)                                                            *C
C*      IDEB      : (I*4) ; print out flag (0,1) = (no print,print).  *C
C* (Outputs)                                                          *C
C*      TIME(  1) : (R*4) ; neutral trigger timing in nsec.           *C
C*          (2-4) : (R*4) ; not used.                                 *C
C*      TYP (  1) : (I*4) ; trigger type bits.                        *C
C*                          bit #    trigger type                     *C
C*                           1       BCL L4                           *C
C*                           2       BCL L3 any two                   *C
C*                           3       ECL L4                           *C
C*                           4       ECL L3 any two                   *C
C*                           5       ECL BCL L3 combination           *C
C*                           6       TPC single endcap                *C
C*                           7       TPC 2 endcaps                    *C
C*                           8       TPC trigger                      *C
C*                           9       LUM                              *C
C*                          10       random                           *C
C*          (  2) : (R*4) ; pre-trigger type bits.                    *C
C*                          bit #    pre-trigger type                 *C
C*                           1       TOF single                       *C
C*                           2       TOF B to B                       *C
C*                           3       IDC R-Phi single                 *C
C*                           4       TOF and IDC R-Phi B to B         *C
C*                           5       R-Z                              *C
C* (Relation)                                                         *C
C*    Called by :                                                     *C
C*    Calls     : PRINFO, PRTRG1, PRTRG2.                             *C
C* (Update Record)                                                    *C
C*    11/27/87  K.Fujii           Original version for handling       *C
C*                                different trigger bank formats.     *C
C*                                                                    *C
CC********************************************************************CC
 
      SUBROUTINE PRTRGT(TYP,TIME,IDEB)
 
      INTEGER*4  IDEB, TYP(2)
      REAL   *4  TIME(4)
C
C========< Entry Point >================================================
C
C--
C  Check experiment #.
C--
      CALL PRINFO(1,IEXP)
C--
C  Call unpacking routines according to experiment #.
C--
      IF ( IEXP.LE.5 ) THEN
         CALL PRTRG1(TYP,TIME,IDEB)
      ELSE
         CALL PRTRG2(TYP,TIME,IDEB)
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
