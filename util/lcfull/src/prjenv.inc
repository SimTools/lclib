CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / PRJENV /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*     Job execution environment.
C*                                                                    *C
C* 2) Update Record                                                   *C
C*    05/02/87  A. Miyamoto  Original Version.                        *C
C*                                                                    *C
CC********************************************************************CC
 
      PARAMETER      (MXxMOD=64)
      COMMON /PRJENV/ NEXTYP(9), MODVRS(MXxMOD), MODDAT(MXxMOD),
     >                NDSCVR,  NDSCDT
      CHARACTER       USERID*8, MACHIN*8, PRGNAM*16, DSCNAM*16
      COMMON /PRJENC/ PRGNAM, USERID, MACHIN, DSCNAM
C
C*    NEXTYP  ; Module type flag.
C*          (1) =
C*          (2) =
C*          (3) = 1/0  Generator module is(is not) incooporated.
C*          (4) = 1/0  Simulator module is(is not) incooporated.
C*          (5) = 1/0  Trigger_Simulator module is(isnot) incooporated.
C*          (6) = 1/0  Formatting module is(is not) incooporated.
C*          (7) = 1/0  Reduction module is(is not) incooporated.
C*          (8) = 1/0  Production module is(is not) incooporated.
C*          (9) = 1/0  Full_Analysis is(is not) incooporated.
C*    MODVRS  ; Version number of the module.
C*    MODDAT  ; Version date of module.
C*    DSCNAM  ; FLANLI routine name.
C*    NDSCVR  ; Version # of FLANLI
C*    NDSCDT  ; Version date of FLANLI
C
C
C=    DATA   PRGNAM/'FULL_ANALYSIS'/
C=    DATA   USERID/'USERXXXX'/
C=    DATA   MACHIN/'KEKFACOM'/
C=    DATA   MODVRS/MXxMOD*0/
C=    DATA   MODDAT/MXxMOD*0/
C=    DATA   DSCNAM/'USER_ANALYSIS'/
C=    DATA   NDSCVR/101/
C=    DATA   NDSCDT/870507/
C*
