CC*********************************************************************C
C*                                                                     *
C*--------------------------------=====                                *
C* SUBROUTINE MAWACC(USERID, DSN, NRET)                                *
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
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAWACC(USERID, DSN, NRET)
C
#include "mames.inc"
C
      CHARACTER*(*) USERID, DSN
      CHARACTER    TRMNAM*8, WDSN*44
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Reject if accessed from TSS system.
C ----------------------------------------------------------------------
C
CTSS       IF(USERID(1:4).EQ.'TAYM')  GO TO 200
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
     >    'TSS system  is not allowed.'
#endif
#ifdef  KEK
        CALL MAxERR(800, 'MAWACC', 1)
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
200   CONTINUE
      WDSN  = DSN
#ifdef  KEK
      IF(WDSN(1:19).EQ.'T#PB.TOPAZ.CONSTANT') THEN
#endif
#ifdef  KEK
        IF(USERID(1:4).NE.'TAYM'.AND.USERID(1:4).NE.'T#ON'.AND.
#endif
#ifdef  KEK
     >     USERID(1:4).NE.'T000'.AND.USERID(1:4).NE.'T#PR'.AND.
#endif
#ifdef  KEK
     >     USERID(1:4).NE.'T#RD'.AND.USERID(1:4).NE.'T@WK'.AND.
#endif
#ifdef  KEK
     >     USERID(1:4).NE.'T#DB')                           THEN
#endif
#ifdef  NGY
      IF(WDSN(1:27).EQ.'TRISTAN.T#PB.TOPAZ.CONSTANT') THEN
#endif
#ifdef  NGY
        IF(USERID(1:7).NE.'TOPAZDA'.AND.
#endif
#ifdef  NGY
     >     USERID(1:7).NE.'PRODUCT'       ) THEN
#endif
#ifdef  NWU
      IF(WDSN(1:27).EQ.'SP63041.T#PB.TOPAZ.CONSTANT') THEN
#endif
#ifdef  NWU
        IF(USERID(1:7).NE.'SP63041'.AND.
#endif
#ifdef  NWU
     >     USERID(1:7).NE.'SP6TOPZ'       ) THEN
#endif
          MESAGE(1) = 'You are not authorized to write access '//
     >                'TOPAZ constant data base file.'
          CALL MAxERR(800, 'MAWACC', 1)
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
