CC********************************************************************CC
C*                                                                    *C
C*====================================----==                          *C
C*  Subroutine UPUTBT(IPRT,ILO,NBT,IX,IOUT)                           *C
C*====================================----==                          *C
C*                                                                    *C
C* (Purpose)                                                          *C
C*   Puts IPRT into IX and returns the results as IOUT.               *C
C* (Inputs)                                                           *C
C*      IPRT = an I*4 number to set in IX.                            *C
C*      ILO  = lowest bit location. (0-31)                            *C
C*      NBT  = number of bits. (1-32)                                 *C
C*      IX   = a I*4 variable into which IPRT is put in.              *C
C* (Output)                                                           *C
C*      IOUT = a I*4 variable with IPRT in specified location.        *C
C* (Update Record)                                                    *C
C*    7/30/85  K. Fujii  Original version                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C ======================================----==
      SUBROUTINE UPUTBT(IPRT,ILO,NBT,IX,IOUT)
C ======================================----==
C  
      DIMENSION MSKHI(0:32),MSKLO(0:32)
#ifdef MSP
      DATA MSKHI
     .  / Z00000000, Z00000001, Z00000003, Z00000007, Z0000000F,
     .               Z0000001F, Z0000003F, Z0000007F, Z000000FF,
     .               Z000001FF, Z000003FF, Z000007FF, Z00000FFF,
     .               Z00001FFF, Z00003FFF, Z00007FFF, Z0000FFFF,
     .               Z0001FFFF, Z0003FFFF, Z0007FFFF, Z000FFFFF,
     .               Z001FFFFF, Z003FFFFF, Z007FFFFF, Z00FFFFFF,
     .               Z01FFFFFF, Z03FFFFFF, Z07FFFFFF, Z0FFFFFFF,
     .               Z1FFFFFFF, Z3FFFFFFF, Z7FFFFFFF, ZFFFFFFFF /
#endif
#ifdef UNIX
      DATA MSKHI
     . /x'00000000',x'00000001',x'00000003',x'00000007',
     . x'0000000F',
     . x'0000001F',x'0000003F',x'0000007F',x'000000FF',
     . x'000001FF',x'000003FF',x'000007FF',x'00000FFF',
     . x'00001FFF',x'00003FFF',x'00007FFF',x'0000FFFF',
     . x'0001FFFF',x'0003FFFF',x'0007FFFF',x'000FFFFF',
     . x'001FFFFF',x'003FFFFF',x'007FFFFF',x'00FFFFFF',
     . x'01FFFFFF',x'03FFFFFF',x'07FFFFFF',x'0FFFFFFF',
     . x'1FFFFFFF',x'3FFFFFFF',x'7FFFFFFF',x'FFFFFFFF' /
#endif
C  
#ifdef  MSP
      DATA MSKLO
     .  / Z00000000, Z80000000, ZC0000000, ZE0000000, ZF0000000,
     .               ZF8000000, ZFC000000, ZFE000000, ZFF000000,
     .               ZFF800000, ZFFC00000, ZFFE00000, ZFFF00000,
     .               ZFFF80000, ZFFFC0000, ZFFFE0000, ZFFFF0000,
     .               ZFFFF8000, ZFFFFC000, ZFFFFE000, ZFFFFF000,
     .               ZFFFFF800, ZFFFFFC00, ZFFFFFE00, ZFFFFFF00,
     .               ZFFFFFF80, ZFFFFFFC0, ZFFFFFFE0, ZFFFFFFF0,
     .               ZFFFFFFF8, ZFFFFFFFC, ZFFFFFFFE, ZFFFFFFFF /
#endif
#ifdef  UNIX
      DATA MSKLO
     ./x'00000000',x'80000000',x'C0000000',x'E0000000',
     . x'F0000000',
     . x'F8000000',x'FC000000',x'FE000000',x'FF000000',
     . x'FF800000',x'FFC00000',x'FFE00000',x'FFF00000',
     . x'FFF80000',x'FFFC0000',x'FFFE0000',x'FFFF0000',
     . x'FFFF8000',x'FFFFC000',x'FFFFE000',x'FFFFF000',
     . x'FFFFF800',x'FFFFFC00',x'FFFFFE00',x'FFFFFF00',
     . x'FFFFFF80',x'FFFFFFC0',x'FFFFFFE0',x'FFFFFFF0',
     . x'FFFFFFF8',x'FFFFFFFC',x'FFFFFFFE',x'FFFFFFFF' /
#endif
C  
      CALL USETBT(IPRT,ILO,NBT,IPAK)
      IMSK  = IOR(MSKHI(ILO),MSKLO(32-NBT-ILO))
      IPAK2 = IAND(IMSK,IX)
      IOUT  = IOR(IPAK2,IPAK)
C  
      RETURN
      END
