CC*********************************************************************C
C*                                                                     *
C*------------------=====                                              *
C* SUBROUTINE MAxOPN(NRET)                                             *
C*------------------=====                                              *
C*                                                                     *
C*(Purpose)                                                            *
C*   Open data base file.
C*                                                                     *
C*(Input)                                                              *
C*   none.
C*                                                                     *
C*(Output)                                                             *
C*   NRET = 1 ; normal return
C*        = 2 ; Data base file is already opened.
C*        =-1 ; Can not open data base with write mode.                *
C*        =-2 ; When user not authorized to open write mode.           *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  13-Mar-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAxOPN(NRET)
C
#include "macntl.inc"
#include "mames.inc"
C
      CHARACTER*8  WAITTM/'00000100'/
      INTEGER*4    MAXWAT/20/
      EXTERNAL     MABLKD
C
C=======< Entry Point >=================================================
C
#ifdef HIUXF77
      CALL MABLKD
#endif
C ----------------------------------------------------------------------
C (1) Initialize.
C ----------------------------------------------------------------------
C
      NRET = 1
      IF(MCxOPS.NE.0) THEN
        MESAGE(1) = 'Data base file is already opened.'
        CALL MAxERR(400,'MAxOPN',1)
        NRET = 2
        RETURN
      ENDIF
      IF(MCxOPN.NE.0) GO TO 300
C
C ----------------------------------------------------------------------
C (2) Read mode open.
C ----------------------------------------------------------------------
C
#ifdef  MSP
      OPEN(UNIT=MCxDBS,FILE=CMxDBS,ACCESS='KEYED',
#endif
#ifdef  MSP
     >     STATUS='SHR',ACTION='READ',IOSTAT=IOS)
#endif
#ifdef  UNIX
      IOS = 0
#endif
      IF(IOS.NE.0) THEN
         NRET = -1
         WRITE(MESAGE(1),210) IOS
210      FORMAT('Can not open data base file, IOSTAT=',I8)
         CALL MAxERR(800, 'MAxOPN', 1)
         STOP
      ENDIF
      MCxOPS = 1
      CALL UIDATE(MCxOPD)
#ifdef  MSP
      CALL   TIME(MCxOPT)
#endif
#ifdef  SUN
      CALL UITIME(MCxOPT)
#endif
      GO TO 900
C
C ----------------------------------------------------------------------
C (3) Write mode open.
C     First Access previledge.
C ----------------------------------------------------------------------
C
300   CONTINUE
      CALL MAWACC(CMxUID, CMxDBS, IRET)
      IF(IRET.NE.1) THEN
         MESAGE(1) = 'You are not authorized to write access '//
     >               'TOPAZ constant data base file.'
         CALL MAxERR(400, 'MAxOPN', 1)
         NRET = -2
         GO TO 900
#ifdef  UNIX
      ELSE
#endif
#ifdef  UNIX
         MESAGE(1) ='Write access to data base on unix system '//
#endif
#ifdef  UNIX
     >              'is not supported yet.'
#endif
#ifdef  UNIX
         CALL MAxERR(400, 'MAxOPN', 1)
#endif
#ifdef  UNIX
         NRET = -2
#endif
#ifdef  UNIX
         GO TO 900
#endif
      ENDIF
C
C
C (3.1) Wait till suceeds to open file.
C
#ifdef  MSP
      CALL ERRSET(169,256,-1,1,1,169)
#endif
      DO 310 IW = 1, MAXWAT
#ifdef  MSP
        OPEN(UNIT=MCxDBS,FILE=CMxDBS,ACCESS='KEYED',
#endif
#ifdef  MSP
     >       STATUS='SHR',ACTION='BOTH',IOSTAT=IOS)
#endif
#ifdef  SUN
        IOS = 0
#endif
        IF(IOS.EQ.0) THEN
          GO TO 330
        ELSE
          MESAGE(1) = 'Data base file not available, will try later'
          CALL MAxERR(400, 'MAxOPN', 1)
#ifdef  MSP
          CALL WAIT(WAITTM)
#endif
        ENDIF
310   CONTINUE
      MESAGE(1) = 'Data base file not available after 20 retry.'
      CALL MAxERR(400, 'MAxOPN', 1)
      NRET   = -1
      GO TO 900
C
C (3.2)
C
330   CONTINUE
      NRET   = 1
      MCxOPS = 1
C
C
900   CONTINUE
      RETURN
      END
