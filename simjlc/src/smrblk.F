#ifdef HIUXF77
      SUBROUTINE SMRBLK
#else
      BLOCK DATA SMRBLK
#endif

#include "smrvgo.inc"
      DATA    NSMPVX /    -3  /
      DATA    DPHIVX / 25.E-4 /
      DATA    DZEEVX / 25.E-4 /
      DATA    RCYVX  / 1.,  2.5,   7.5,  25., 7*0. /
      DATA    ZCYVX  / -100., 100.,
     .                   -7.5,   7.5,
     .                  -22.5,  22.5,
     .                 -230., 230.,
     .                  14*0      /
      DATA    RDLVX  / 0.003, 0.003, 0.003, 0.01, 7*0. /
      DATA    NERRVX /1/
      DATA    ERRVX  / 4.E-4, 4*0.0/
      DATA    NUMVTX / 2/
#include "smrvtp.inc"
      DATA    NXMPCV /3/
      DATA    NSMPVT /4/
      DATA    DSMPVT /0.0/
      DATA    RCYVT  /2.4, 6.0/
      DATA    ZCYVT  /-12.5, 12.5/

#include "smrtgo.inc"
      DATA    NSMPCT /   10  /
      DATA    NSMPTK /  100  /
      DATA    DSMPTK /    0. /
      DATA    RCYTK  /   30., 230. /
      DATA    ZCYTK  / -230., 230. /
#include "smrego.inc"
      DATA    NPHIEM /  156  /
      DATA    NTHTEM /   50  /
      DATA    NRADEM /   18  /
      DATA    NPADEM /   10  /
      DATA    DPHIEM /    0. /
      DATA    DTHTEM /    0. /
      DATA    DRDMEM /    0. /
      DATA    DRDPEM /    0. /
      DATA    THMNEM /    0. /
      DATA    THMXEM /    0. /
      DATA    RMMNEM /    0. /
      DATA    RMMXEM /    0. /
      DATA    RPMNEM /    0. /
      DATA    RPMXEM /    0. /
      DATA    RCYEM  /   45., 250. /
      DATA    ZCYEM  / -300., 300. /
#include "smrhgo.inc"
      DATA    NPHIHD /   78  /
      DATA    NTHTHD /   25  /
      DATA    NRADHD /    9  /
      DATA    DPHIHD /    0. /
      DATA    DTHTHD /    0. /
      DATA    DRDMHD /    0. /
      DATA    DRDPHD /    0. /
      DATA    THMNHD /    0. /
      DATA    THMXHD /    0. /
      DATA    RMMNHD /    0. /
      DATA    RMMXHD /    0. /
      DATA    RPMNHD /    0. /
      DATA    RPMXHD /    0. /
      DATA    RCYHD  /  48.0, 270. /
      DATA    ZCYHD  / -324., 324. /
#include "smrres.inc"
      DATA             SIGRF    / 1.E-2 /
     .                 SIGZE    / 3.E-1 /
     .                 SGEMB0   / 1.0E-2 /
     .                 SGEMB    / 1.5E-1 /
     .                 SGEME0   / 1.0E-2 /
     .                 SGEME    / 1.5E-1 /
     .                 SGHDB0   / 1.0E-2 /
     .                 SGHDB    / 4.0E-1 /
     .                 SGHDE0   / 1.0E-2 /
     .                 SGHDE    / 4.0E-1 /
#include "smrrnd.inc"
      DATA ISEED / 314159 /
#include "smrfld.inc"
      DATA IMODBF / 2*0  /
      DATA BFDATA / 2*0. /
#include "smrclp.inc"
      DATA (CLSPAR(I,1),I=1,5)/ 100., 0.40, 0.20, 1.00, 0.3 /
      DATA (CLSPAR(I,2),I=1,5)/ 100., 0.40, 0.20, 1.00, 0.3 /
      DATA (CLSPAR(I,3),I=1,5)/ 100., 0.40, 0.20, 1.00, 0.25 /
      DATA (CLSPAR(I,4),I=1,5)/ 100., 0.40, 0.20, 1.00, 0.25 /
      END
