CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* Subroutine PRJTIT                                                   *
C*====================                                                 *
C*  (Purpose)                                                          *
C*       Write Production job title heading.                           *
C*  (Relation)                                                         *
C*       Called by                                                     *
C*           PRJOBI                                                    *
C*  (Author)                                                           *
C*       A. Miyamoto    15-Nov-1985  Original version.                 *
C*                                                                     *
CC**********************************************************************
C
      SUBROUTINE PRJTIT
C
      CHARACTER*8  HIZUKE,JIKAN,VDATE
#include "prjenv.inc"
C
C
C==================<<  Entry Point  >>================================
C
C
C ------------------- Write Job heading
C
      CALL DATE(HIZUKE)
      CALL UTIME(JIKAN)
      CALL PRGVRS(NVERS, NVERSL, VDATE)
      WRITE(6,100) PRGNAM, NVERS, NVERSL, VDATE, HIZUKE, JIKAN
100   FORMAT(1H1,///,
     > ' ************************************************',
     > '******************************',/,
     > ' **',74X,'**',/,
#ifdef TOPAZ
     > ' **   TOPAZ ',A,
#endif
#ifdef  JLC
     > ' **     JLC ',A,
#endif
     > 16X,'( Version ',I2,'.',I2.2,' Date ',A8,' )',
     >  2X,'**',/,
     > ' **',74X,'**',/,
     > ' **         Date ',A8,52X,'**',/,
     > ' **         Time ',A8,52X,'**',/,
     > ' **',74X,'**',/,
     > ' ************************************************',
     > '******************************',///)
C
      RETURN
      END
