CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / USKALR /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*     This COMMON contains error counters for TOPAZ offline codes.   *C
C*       MXxSKL      : # counters used in the job.                    *C
C*       ISKLR(ID,1) : # errors accumulated on ID-th counter(Reduct)  *C
C*            (ID,2) : # errors accumulated on ID-th counter(Product) *C
C*                     This is zapped by UERINI and set by UERROR.    *C
C* 2) Update Record                                                   *C
C*     10/14/85  K. Fujii  Original version.                          *C
C*     05/27/86  K. Fujii  ID = 0 is added.                           *C
C*     05/01/87  K. Fujii  JSKLR  is added.                           *C
C*     05/06/87  K. Fujii  JSKLR  is deleted. ISKLR is now 2-dim.     *C
C*                         MXxSKL is extended to 129.                 *C
C*                                                                    *C
CC********************************************************************CC
   
      PARAMETER ( MXxSKL=129 )
      COMMON /USKALR/ ISKLR(0:MXxSKL,2)
