CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / UJBTYP /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*     This COMMON contains job type information.                     *C
C*       JBTYP       : job type.                                      *C
C*                   : (1,2,3) = (simulation,reduction,production)    *C
C*     This COMMON is set by UERINI once per job.                     *C
C* 2) Update Record                                                   *C
C*    10/14/85  K. Fujii  Original version.                           *C
C*                                                                    *C
CC********************************************************************CC
C  
C ===========================
      COMMON /UJBTYP/ JBTYP
C ===========================
