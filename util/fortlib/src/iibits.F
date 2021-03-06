CC********************************************************************CC
C*                                                                    *C
C*===================--------============                             *C
C*  Integer*2 Function  IIBITS(IX,ILO,NBT)                            *C
C*===================--------============                             *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   Extracts a contiguous part of bits from input I*2 variables.     *C
C*   Same algorithm as UTAKBT.  Same calling sequence as VAX FORTRAN  *C
C*   IBITS.                                                           *C
C*                                                                    *C
C* (Inputs)                                                           *C
C*      IX   = a I*2 variable to extract some bits from.              *C
C*      ILO  = lowest bit location. (0-15)                            *C
C*      NBT  = number of bits. (1-16)                                 *C
C* (Output)                                                           *C
C*      IBITS= results as I*2 variable.                               *C
C* (Update Record)                                                    *C
C*    7/30/85  K. Fujii  Original version                             *C
C*   11/29/85  A. Miyamoto  Modified from UTAKBT                      *C
C*                                                                    *C
CC********************************************************************CC
C  
C =======================-------=============
      FUNCTION IIBITS(IX, ILO, NBT )
C =======================-------=============
C  
      INTEGER*2 IIBITS,IPRT,MASK(16),IX
#ifdef  MSP
           DATA MASK / Z0001, Z0003, Z0007, Z000F,
          .            Z001F, Z003F, Z007F, Z00FF,
          .            Z01FF, Z03FF, Z07FF, Z0FFF,
          .            Z1FFF, Z3FFF, Z7FFF, ZFFFF/
#endif
#ifdef  UNIX
            DATA MASK / x'0001',x'0003',x'0007',x'000F',
     .           x'001F',x'003F',x'007F',x'00FF',
     .           x'01FF',x'03FF',x'07FF',x'0FFF',
     .           x'1FFF',x'3FFF',x'7FFF',x'FFFF'/
#endif
C  
#if defined(HIUXF77) || defined(JavaVM)
      IIX = IX
      IPRT = ISHFT(IIX,-ILO)
      KMSK = MASK(NBT)
      KPRT = IPRT
      IPRT = IAND(KMSK,KPRT)
#else
      IPRT = ISHFT(IX,-ILO)
      IPRT = IAND(MASK(NBT),IPRT)
#endif
      IIBITS = IPRT
C  
      RETURN
      END

