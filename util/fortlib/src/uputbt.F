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
     . /'00000000'X,'00000001'X,'00000003'X,'00000007'X,
     . '0000000F'X,
     . '0000001F'X,'0000003F'X,'0000007F'X,'000000FF'X,
     . '000001FF'X,'000003FF'X,'000007FF'X,'00000FFF'X,
     . '00001FFF'X,'00003FFF'X,'00007FFF'X,'0000FFFF'X,
     . '0001FFFF'X,'0003FFFF'X,'0007FFFF'X,'000FFFFF'X,
     . '001FFFFF'X,'003FFFFF'X,'007FFFFF'X,'00FFFFFF'X,
     . '01FFFFFF'X,'03FFFFFF'X,'07FFFFFF'X,'0FFFFFFF'X,
     . '1FFFFFFF'X,'3FFFFFFF'X,'7FFFFFFF'X,'FFFFFFFF'X /
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
     ./'00000000'X,'80000000'X,'C0000000'X,'E0000000'X,
     . 'F0000000'X,
     . 'F8000000'X,'FC000000'X,'FE000000'X,'FF000000'X,
     . 'FF800000'X,'FFC00000'X,'FFE00000'X,'FFF00000'X,
     . 'FFF80000'X,'FFFC0000'X,'FFFE0000'X,'FFFF0000'X,
     . 'FFFF8000'X,'FFFFC000'X,'FFFFE000'X,'FFFFF000'X,
     . 'FFFFF800'X,'FFFFFC00'X,'FFFFFE00'X,'FFFFFF00'X,
     . 'FFFFFF80'X,'FFFFFFC0'X,'FFFFFFE0'X,'FFFFFFF0'X,
     . 'FFFFFFF8'X,'FFFFFFFC'X,'FFFFFFFE'X,'FFFFFFFF'X /
#endif
C  
      CALL USETBT(IPRT,ILO,NBT,IPAK)
      IMSK  = IOR(MSKHI(ILO),MSKLO(32-NBT-ILO))
      IPAK2 = IAND(IMSK,IX)
      IOUT  = IOR(IPAK2,IPAK)
C  
      RETURN
      END
