#ifdef HIUXF77
      BLOCK DATA mablkd_
#else
      BLOCK DATA MABLKD
#endif
#include "macntl.inc"
      DATA  MCxDBS/80/
      DATA  MCxOPN/ 0/
      DATA  MCxKEY/20/
      DATA  MCxKYL/16/
      DATA  MCxVSM/10000/
      DATA  MCxOPS/0/
      DATA  MCxOPD/0/
      DATA  MCxOPT/0/
      DATA  MCxMPT/6/
      DATA  MCxMGT/5/
      DATA  MCxPRT/77/
      DATA  MCxMSL/100/
#ifdef  KEK
      DATA  CMxDBS/'T#ON.TOPAZ.RUNLIST'/
#endif
#ifdef  NGY
      DATA  CMxDBS/'TRISTAN.T#ON.TOPAZ.RUNLIST'/
#endif
#ifdef  NWU
      DATA  CMxDBS/'SP63041.T#ON.TOPAZ.RUNLIST'/
#endif
      DATA  CMxJBN/'XXXXXXXXUSERJOB'/
      DATA  CMxVRS/'(I4.4)'/
      DATA  CMxBLK/'  '/
      DATA  CMxDLM
     >  /'9999999999999999999999999999999999999999999999999999999999'/
#include "maslct.inc"
CC      PARAMETER (MXxSLC=22)
#ifdef  MSP
      DATA MSxSLC/ Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF,      -998, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF,
#endif
#ifdef  MSP
     >             Z80000000, Z7FFFFFFF, Z80000000, Z7FFFFFFF/
#endif
#ifdef  UNIX
      DATA MSxSLC/X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',       -998,X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF',
#endif
#ifdef  UNIX
     >    X'80000000',X'7FFFFFFF',X'80000000',X'7FFFFFFF'/
#endif
      END
