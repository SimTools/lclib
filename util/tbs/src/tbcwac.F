CC*********************************************************************C
C*                                                                     *
C*--------------------------------=====                                *
C* SUBROUTINE TBCWAC(USERID, DSN, NRET)                                *
C*--------------------------------=====                                *
C*                                                                     *
C*(Purpose)                                                            *
C*   Check the priviledge of write mode access.
C*                                                                     *
C*(Input)                                                              *
C*   USERID  : User ID.
C*   DSN     : Data base file name.
C*                                                                     *
C*(Output)                                                             *
C*   NRET = 1 ; Write mode access is allowed.
C*        =-1 ; Write mode access is not alowed.                       *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  13-Mar-1987       Original version.                  *
C*                 8-Jul-1987       Compatible to the NAGOYA           *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE TBCWAC(USERID, DSN, NRET)
C
#include "tbsmes.inc"
C
      CHARACTER*(*) USERID, DSN
      CHARACTER    TRMNAM*8, WDSN*44
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (0) Check that program is compiled correctly.
C ----------------------------------------------------------------------
C
      ITEST = 0
#ifdef  KEK
      ITEST = 1
#endif
#ifdef  NGY
      ITEST = 2
#endif
#ifdef  NWU
      ITEST = 3
#endif
      IF( ITEST .EQ. 0 ) THEN
        PRINT *,'%Error TBCWAC .. TBS module was not compiled ',
     >          ' correctly.  Use R(KEK) or R(NGY) in BUILD comand.'
        PRINT *,'                 ask your instalation manager.',
     >          ' for details. '
        STOP
      ELSEIF( ITEST .EQ. 2 ) THEN
        IF( USERID(5:8) .EQ. '    ' ) THEN
          LUID = LEN(USERID)
          PRINT *,'%Error TBCWAC .. User ID (',USERID(:LUID),
     >            ' is invalid.'
          PRINT *,'Use correct DSNCHK module.'
          STOP
        ENDIF
      ENDIF
C
C ----------------------------------------------------------------------
C (1) Reject if accessed from TSS system.
C ----------------------------------------------------------------------
C
#ifdef  KEK
      CALL TERMID(TRMNAM)
#endif
#ifdef  KEK
      IF(TRMNAM(1:8).NE.'        ') THEN
#endif
#ifdef  KEK
        MESAGE(1) = 'Write access to the data base from '//
#endif
#ifdef  KEK
     >               'TSS system is not allowed'
#endif
#ifdef  KEK
        CALL TBxERR(800, 'TBCWAC', 1)
#endif
#ifdef  KEK
        STOP
#endif
#ifdef  KEK
      ENDIF
#endif
C
C ----------------------------------------------------------------------
C (2) Reject, if user going to access TOPAZ standard data base file.
C ----------------------------------------------------------------------
C
      WDSN  = DSN
      IF(WDSN(1:19).EQ.'T#DB.TOPAZ.CONSTANT') THEN
        IF(USERID(1:4).NE.'TAYM'.AND.USERID(1:4).NE.'T#ON'.AND.
     >     USERID(1:4).NE.'T000'.AND.USERID(1:4).NE.'T#PR'.AND.
     >     USERID(1:4).NE.'T#RD'.AND.USERID(1:4).NE.'T@WK'.AND.
     >     USERID(1:4).NE.'T#DB')                           THEN
          MESAGE(1) = 'You are not authorized to write access '//
     >                'TOPAZ constant data base file.'
          MESAGE(2) = ' User ID ='//USERID
          MESAGE(3) = ' DSN     ='//WDSN
          CALL TBxERR(800, 'TBCWAC', 3)
          STOP
        ENDIF
      ELSEIF(WDSN(1:27).EQ.'TRISTAN.T#DB.TOPAZ.CONSTANT') THEN
        IF(USERID(1:7).NE.'TOPAZDA'.AND.USERID(1:7).NE.'PRODUCT') THEN
          MESAGE(1) = 'You are not authorized to write access '//
     >                'TOPAZ constant data base file.'
          MESAGE(2) = ' User ID ='//USERID
          MESAGE(3) = ' DSN     ='//WDSN
          CALL TBxERR(800, 'TBCWAC', 3)
          STOP
        ENDIF
      ENDIF
 
C
C ----------------------------------------------------------------------
C (3) Set return flag, then return.
C ----------------------------------------------------------------------
C
      NRET = 1
      RETURN
      END
