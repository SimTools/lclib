CC*********************************************************************C
C*                                                                     *
C*------------------                                                   *
C* SUBROUTINE TBCCLS(NRET)                                             *
C*------------------                                                   *
C*                                                                     *
C*(Purpose)                                                            *
C*   Close data base file.                                             *
C*                                                                     *
C*(Input)                                                              *
C*   none.                                                             *
C*                                                                     *
C*(Output)                                                             *
C*   NRET  = 1 : Normal return                                         *
C*         = 2 : Data base file is not opened.                         *
C*         =-1 : Failed to close data base file.                       *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  17-Mar-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE TBCCLS(NRET)
C
#include "tbcctl.inc"
#include "tbsmes.inc"
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Close data base file.
C ----------------------------------------------------------------------
C
      NRET  = 1
      IF(MCxOPS.EQ.0) THEN
        MESAGE(1) = 'Data base file is not opened yet.'
        CALL TBxERR(400, 'TBCCLS', 1)
        NRET = 2
        RETURN
      ENDIF
C
      CLOSE(MCxDBS,IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        WRITE(MESAGE(1), 100) IOS
100     FORMAT('Failed to close data base file, IOS=',I8)
        CALL TBxERR(400, 'TBCCLS', 1)
        NRET = -1
      ELSE
        MCxOPS = 0
      ENDIF
C
      RETURN
      END
