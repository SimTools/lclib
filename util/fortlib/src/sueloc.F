CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine SUELOC                                                  *C
C*====================                                                *C
C*                                                                    *C
C*  1) Purpose                                                        *C
C*       Tells when error happened in simulator.                      *C
C*  2) Relation                                                       *C
C*       Called by;                                                   *C
C*           UERLOC : locates error.                                  *C
C*       Calls;                                                       *C
C*           SSRPAR : gives event information for Simulator.          *C
C*  4) Update record                                                  *C
C*       09/14/85  K.Fujii  Original version.                         *C
C*                                                                    *C
CC********************************************************************CC
C
C =========================
      SUBROUTINE SUELOC
C =========================
C
C
C -------------------------------------------------------------------- C
C     Write out event informations to help debugging.                  C
C -------------------------------------------------------------------- C
C
#if defined (TOPAZ)
C
C --- Get event#.
C
      CALL SSRPAR('NOWEV',NEVENT)
C
C --- Print out current seed values.
C
      CALL SSWSED( -NEVENT )
C
#endif
#if defined (JLC)
      PRINT *,'Dummy SUELOC is called.'
#endif
      RETURN
      END
