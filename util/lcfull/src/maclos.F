CC*********************************************************************C
C*                                                                     *
C*------------------                                                   *
C* SUBROUTINE MACLOS                                                   *
C*------------------                                                   *
C*                                                                     *
C*(Purpose)                                                            *
C*   Close data base file.                                             *
C*                                                                     *
C*(Input)                                                              *
C*   none.                                                             *
C*                                                                     *
C*(Output)                                                             *
C*   none.                                                             *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  17-Mar-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MACLOS
C
#include "macntl.inc"
#include "mames.inc"
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
        CALL MAxERR(400, 'MACLOS', 1)
        NRET = 2
        RETURN
      ENDIF
C
      CLOSE(MCxDBS,IOSTAT=IOS)
      IF(IOS.NE.0) THEN
        WRITE(MESAGE(1), 100) IOS
100     FORMAT('Failed to close data base file, IOS=',I8)
        CALL MAxERR(400, 'MACLOS', 1)
        NRET = -1
      ELSE
        MCxOPS = 0
      ENDIF
C
      RETURN
      END
