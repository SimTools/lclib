CC*********************************************************************C
C*                                                                     *
C*------------------=====                                              *
C* SUBROUTINE TBCxOP(NRET)                                             *
C*------------------=====                                              *
C*                                                                     *
C*(Purpose)                                                            *
C*   Open Calibration constant file.
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
C*                 8-Jul-1987       Put protection to avoid wrong comp.*
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE TBCxOP(NRET)
C
#include "tbcctl.inc"
#include "tbsmes.inc"
C
      CHARACTER*8  WAITTM/'00000100'/
      INTEGER*4    MAXWAT/20/
      EXTERNAL     TBCBLK
#ifdef UNIX
	character*128 dbsdir
#endif

C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (0) Confirm that this module is cmpiled correctly.
C ----------------------------------------------------------------------
C
      ITEST = 0
#ifdef  KEK
      ITEST = 1
#endif
#ifdef  NGY
      ITEST = 1
#endif
#ifdef  NWU
      ITEST = 1
#endif
      IF( ITEST .EQ. 0 ) THEN
        PRINT *,'%Error TBCxOP .. TBS module was not compiled ',
     >          ' correctly.  Use R(KEK) or R(NGY) in BUILD comand.'
        PRINT *,'                 ask your instalation manager.',
     >          ' for details. '
        STOP
      ENDIF
C
C ----------------------------------------------------------------------
C (1) Initialize.
C ----------------------------------------------------------------------
C
      NRET = 1
      IF(MCxOPS.NE.0) THEN
        MESAGE(1) = 'Data base file is already opened.'
        CALL TBxERR(400,'TBCxOP',1)
        NRET = 2
        RETURN
      ENDIF
      IF(MCxOPN.NE.0) GO TO 300
C
C ----------------------------------------------------------------------
C (2) Read mode open.
C ----------------------------------------------------------------------
C
#ifdef UNIX
#ifndef HIUXF77
	call getenv('TBS_CONSTANTS_DBS_DIR',dbsdir)
#else
	call getenv('TBS_CONSTANTS_DBS_DIR',21,dbsdir,128)
#endif
	if( dbsdir(1:1).ne.' ') cmxdbs = dbsdir
	ios = 0
#endif
#ifdef  MSP
      OPEN(UNIT=MCxDBS,FILE=CMxDBS,ACCESS='KEYED',
     >     STATUS='SHR',ACTION='READ',IOSTAT=IOS)
      IF(IOS.NE.0) THEN
         NRET = -1
         WRITE(MESAGE(1),210) IOS
210      FORMAT('Can not open data base file, IOSTAT=',I8)
         CALL TBxERR(800, 'TBCxOP', 1)
         STOP
      ENDIF
#endif
      MCxOPS = 1
      CALL UIDATE(MCxOPD)
      CALL   TIME(MCxOPT)
      GO TO 900
C
C ----------------------------------------------------------------------
C (3) Write mode open.
C     First Access previledge.
C ----------------------------------------------------------------------
C
300   CONTINUE
#ifdef UNIX
#ifndef HIUXF77
	call getenv('TBS_CONSTANTS_DBS_DIR',dbsdir)
#else
	call getenv('TBS_CONSTANTS_DBS_DIR',21,dbsdir,128)
#endif
	if( dbsdir(1:1).ne.' ') cmxdbs = dbsdir
	ios = 0
	go to 330
#endif
#ifdef MSP
      CALL TBCWAC(CMxUID, CMxDBS, IRET)
      IF(IRET.NE.1) THEN
         MESAGE(1) = 'You are not authorized to write access '//
     >               'TOPAZ constant data base file.'
         CALL TBxERR(400, 'TBCxOP', 1)
         NRET = -2
         GO TO 900
      ENDIF
C
C
C (3.1) Wait till suceeds to open file.
C
      CALL ERRSET(169,256,-1,1,1,169)
      DO 310 IW = 1, MAXWAT
        OPEN(UNIT=MCxDBS,FILE=CMxDBS,ACCESS='KEYED',
     >       STATUS='SHR',ACTION='BOTH',IOSTAT=IOS)
        IF(IOS.EQ.0) THEN
          GO TO 330
        ELSE
          MESAGE(1) = 'Data base file not available, will try later'
          CALL TBxERR(400, 'TBCxOP', 1)
          CALL WAIT(WAITTM)
        ENDIF
310   CONTINUE
      MESAGE(1) = 'Data base file not available after 20 retry.'
      CALL TBxERR(400, 'TBCxOP', 1)
      NRET   = -1
      GO TO 900
#endif
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
