CC********************************************************************CC
C*                                                                    *C
C*===========----========                                             *C
C*  Function UTEE(PBM,P)                                              *C
C*===========----========                                             *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   Calculates TEE variables.                                        *C
C*         TEE = (P-PBEAM)*(P-PBEAM)                                  *C
C* (Inputs)                                                           *C
C*      PBM(4) = beam 4-momentum.                                     *C
C*      P  (4) = 4-momentum of a particle in question.                *C
C* (Output)                                                           *C
C*      UTEE   = tee of P and PBM.                                    *C
C* (Relation)                                                         *C
C*   Calls                                                            *C
C*      USUB4, UDOT4                                                  *C
C* (Update Record)                                                    *C
C*   7/29/85  K. Fujii   Original version.                            *C
C*                                                                    *C
CC********************************************************************CC
C  
C =============----========
      FUNCTION UTEE(PBM,P)
C =============----========
C  
      DIMENSION PBM(4),P(4),PUTEE(4)
      CALL USUB4(PBM,P,PUTEE)
      UTEE = UDOT4(PUTEE,PUTEE)
      RETURN
      END
