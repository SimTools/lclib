CC********************************************************************CC
C*                                                                    *C
C*===================--------============                             *C
C*  Integer*4 Function  JIBITS(IX,ILO,NBT)                            *C
C*===================--------============                             *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   Extracts a contiguous part of bits from input I*4 variables.     *C
C*   Same algorithm as UTAKBT.  Same calling sequence as VAX FORTRAN  *C
C*   IBITS.                                                           *C
C*                                                                    *C
C* (Inputs)                                                           *C
C*      IX   = a I*4 variable to extract some bits from.              *C
C*      ILO  = lowest bit location. (0-31)                            *C
C*      NBT  = number of bits. (1-32)                                 *C
C* (Output)                                                           *C
C*      IBITS= results as I*4 variable.                               *C
C* (Update Record)                                                    *C
C*    7/30/85  K. Fujii  Original version                             *C
C*   11/29/85  A. Miyamoto  Modified from UTAKBT                      *C
C*                                                                    *C
CC********************************************************************CC
C  
C =======================-------=============
      FUNCTION JIBITS(IX, ILO, NBT )
C =======================-------=============
C  
      INTEGER*4   JIBITS,MASK(32)
#ifdef  MSP
       DATA MASK / Z00000001, Z00000003, Z00000007, Z0000000F,
#endif
#ifdef  MSP
      .            Z0000001F, Z0000003F, Z0000007F, Z000000FF,
#endif
#ifdef  MSP
      .            Z000001FF, Z000003FF, Z000007FF, Z00000FFF,
#endif
#ifdef  MSP
      .            Z00001FFF, Z00003FFF, Z00007FFF, Z0000FFFF,
#endif
#ifdef  MSP
      .            Z0001FFFF, Z0003FFFF, Z0007FFFF, Z000FFFFF,
#endif
#ifdef  MSP
      .            Z001FFFFF, Z003FFFFF, Z007FFFFF, Z00FFFFFF,
#endif
#ifdef  MSP
      .            Z01FFFFFF, Z03FFFFFF, Z07FFFFFF, Z0FFFFFFF,
#endif
#ifdef  MSP
      .            Z1FFFFFFF, Z3FFFFFFF, Z7FFFFFFF, ZFFFFFFFF /
#endif
#ifdef  UNIX
       DATA MASK/x'00000001',x'00000003',x'00000007',x'0000000F',
#endif
#ifdef  UNIX
     .     x'0000001F',x'0000003F',x'0000007F',x'000000FF',
#endif
#ifdef  UNIX
     .     x'000001FF',x'000003FF',x'000007FF',x'00000FFF',
#endif
#ifdef  UNIX
     .     x'00001FFF',x'00003FFF',x'00007FFF',x'0000FFFF',
#endif
#ifdef  UNIX
     .     x'0001FFFF',x'0003FFFF',x'0007FFFF',x'000FFFFF',
#endif
#ifdef  UNIX
     .     x'001FFFFF',x'003FFFFF',x'007FFFFF',x'00FFFFFF',
#endif
#ifdef  UNIX
     .     x'01FFFFFF',x'03FFFFFF',x'07FFFFFF',x'0FFFFFFF',
#endif
#ifdef  UNIX
     .     x'1FFFFFFF',x'3FFFFFFF',x'7FFFFFFF',x'FFFFFFFF' /
#endif
C  
      IPRT = ISHFT(IX,-ILO)
      IPRT = IAND(MASK(NBT),IPRT)
      JIBITS = IPRT
C  
      RETURN
      END

