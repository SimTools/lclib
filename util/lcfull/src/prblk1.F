#ifdef HIUXF77
      SUBROUTINE PRBLK1
#else
      BLOCK DATA PRBLK1
#endif
C*
C*  Data for Default parameters.
C*
#include "prjctl.inc"
      DATA   RTIMLM/15.0/
      DATA   RCPUMX/99999.999/
      DATA   NEVMAX/999999999/
      DATA   NDFREQ/        1/
      DATA   IEVSEQ/        0/
      DATA   NFSTEV/        1/
      DATA   NLSTEV/999999999/
      DATA   NFSTDB/        1/
      DATA   NLSTDB/999999999/
      DATA   NFSTOT/        1/
      DATA   NLSTOT/999999999/
      DATA   KLLVLR/       -9/
      DATA   KLLVLP/       -9/
      DATA   NFSTEX/-99999999/
      DATA   NLSTEX/ 99999999/
      DATA   NFSTRN/-99999999/
      DATA   NLSTRN/ 99999999/
#include "prfctl.inc"
      DATA    NINUNI/10/
      DATA    NINFIL/ 1/
      DATA    IINFIL/ 1/
      DATA    NOTUNI/21/
      DATA    NOTFIL/ 1/
      DATA    IOTFIL/ 1/
      DATA    NOTDEV/ 1/
      DATA    XOSIZE/ -1./
      DATA    ZOSIZE/  0./
#include "prjflg.inc"
      DATA (NJBDBG(K,1),K=1,30) /0, 8, 28*0/
      DATA (NJBLVL(K,1),K=1, 5)
     ./  1,  3, 3*0/
      DATA (NJBLVL(K,1),K=6, 20)
     ./  3,  2,   2,  1,  1,  1,  0,   1,  1,  0, 0,   1,   1,   1,  1/
      DATA (NJBLVL(K,1),K=21,30)
     ./  1,  1,   1,  1,  6*0/
      DATA (NJBDBG(K,2),K=1,30) /30*0/
      DATA (NJBLVL(K,2),K=1, 5)
     ./  1,  1, 3*0/
      DATA (NJBLVL(K,2),K=6, 20)
     ./  2,  2,   2,  0,  2,  2,  0,   2,  2,  2, 0,  1 ,   0,   0,  0/
      DATA (NJBLVL(K,2),K=21,30)
     ./  1,  1,   2,  1,  6*0/
      DATA (NJBDBG(K,3),K=1,30) /30*0/
      DATA (NJBLVL(K,3),K=1,30) /30*0/
#include "prjenv.inc"
      DATA   PRGNAM/'FULL_ANALYSIS'/
      DATA   USERID/'USERXXXX'/
      DATA   MACHIN/'KEKFACOM'/
      DATA   MODVRS/MXxMOD*0/
      DATA   MODDAT/MXxMOD*0/
      DATA   DSCNAM/'USER_ANALYSIS'/
      DATA   NDSCVR/101/
      DATA   NDSCDT/870507/
#include "prcnsv.inc"
#ifdef UNIX
      DATA CNFILES/43*' '/
#endif
      DATA NPRTCN/2/
      DATA NCNVRS/43*0/
#include "prunls.inc"
      DATA            LRUNLS/MXxRLW*-999/
#include "prgenf.inc"
#include "prjpr1.inc"
      DATA  NDELBK/ 1 /
      DATA  NALLWT/ 1 /
C
C*
C*  Momentum -- Radius conversion factor.
C*
#include "prfeld.inc"
      DATA            BFIELD/10.1085E0/
      DATA            PTOR  /329.9837713/
      DATA            ROPT  /3.03045206E-3/
C
      END
