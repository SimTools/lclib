CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / PREERR /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*     This COMMON contains error counters at the begining of the .   *C
C*     events.
C*
C*     ISKLRR is the value at the begining of RUN.
C* 2) Update Record                                                   *C
C*     05/06/87  A. Miyamoto  Original version.
C*                                                                    *C
CC********************************************************************CC
 
      COMMON /PREERR/ ISKLRE(0:MXxSKL, 2),
     >                ISKLRR(0:MXxSKL, 2)
