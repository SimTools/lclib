#ifdef HIUXF77
      BLOCK DATA tbcblk_
#else
      BLOCK DATA TBCBLK
#endif
#include "tbcctl.inc"
C      DATA  MCxDBS/-1/
#ifdef  KEK
      DATA  MCxDBS/78/
#endif
#ifdef  NGY
      DATA  MCxDBS/78/
#endif
#ifdef  NWU
      DATA  MCxDBS/78/
#endif
      DATA  MCxOPN/ 0/
C      DATA  MCxKEY/999999/
#ifdef  NGY
      DATA  MCxKEY/50/
#endif
#ifdef  KEK
      DATA  MCxKEY/50/
#endif
#ifdef  NWU
      DATA  MCxKEY/50/
#endif
      DATA  MCxKYL/44/
      DATA  MCxVSM/100000/
      DATA  MCxOPS/0/
      DATA  MCxOPD/0/
      DATA  MCxOPT/0/
C      DATA  CMxDBS/'XXXXXXXXXXXXXXXXXXX'/
#ifdef  MSP
      DATA  CMxDBS/'T#DB.TOPAZ.CONSTANT'/
#endif
C .. Default data base file for UNIX system.
C .. Over written by Environment variable TBS_CONSTANTS_DBS_DIR, if set. 
#ifdef  UNIX
      DATA  CMxDBS/'/home/topsun1/miyamoto/topaz/jclfull/dbs/constants'/
#endif
#ifdef  NGY
      DATA  CMxDBS/'TRISTAN.T#DB.TOPAZ.CONSTANT'/
#endif
#ifdef  NWU
      DATA  CMxDBS/'SP63041.T#DB.TOPAZ.CONSTANT'/
#endif
      DATA  CMxJBN/'XXXXXXXXUSERJOB'/
      DATA  CMxVRS/'(I5.5)'/
      DATA  CMxBLK/'  '/
      DATA  CMxDLM
     >  /'9999999999999999999999999999999999999999999999999999999999'/
      END
