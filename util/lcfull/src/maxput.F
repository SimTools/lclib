CC*********************************************************************C
C*                                                                     *
C*-------------------------------------------------------=====         *
C* SUBROUTINE MAxPUT(MODE, NKEY, MXxSIZ, KEY, NW, NDATA, NRET)         *
C*-------------------------------------------------------=====         *
C*                                                                     *
C*(Purpose)                                                            *
C*   Put data to the data base master file.                            *
C*                                                                     *
C*(Input)                                                              *
C*   MODE  ; Write mode                                                *
C*       0 = Confirm that same key exists or not and saved data as     *
C*          highest version # data.                                    *
C*       1 = Does not confirm, write them as new KEY data.             *
C*       2 = Does not add version number.  Length of KEY should be     *
C*           equal to MCxKEY.                                          *
C*                                                                     *
C*   NKEY  ; # of keys                                                 *
C*   MXxSIZ; Length of each data record.                               *
C*   KEY(NKEY) ; Key string.                                           *
C*   NW(NKEY)  ; # of words of each data elements.                     *
C*   NDATA(NKEY) ; Data array.                                         *
C*                                                                     *
C*(Output)                                                             *
C*   NRET = 0 ; normal return                                          *
C*        = 1 ; MODE = 1 case, same key already exists. data is saved  *
C*             as highest version one.                                 *
C*        =-1 ; Write error detected.                                  *
C*        =-2 ; User is not authorized to use this routine.            *
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto   9-Jan-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MAxPUT(MODE, NKEY, MXxSIZ, KEY, NW, NDATA, NRET)
C
      CHARACTER*(*) KEY(NKEY)
      INTEGER*4     NW(NKEY), NDATA(MXxSIZ, NKEY)
C
#include "macntl.inc"
#include "maxwrk.inc"
C
      CHARACTER    MESAGE*137
      CHARACTER*68 KEYTMP, KEYGET
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Initialize parameter.
C ----------------------------------------------------------------------
C
      NRET = 0
#ifdef  UNIX
      NRET = 1
      IF( NRET.EQ.1 ) THEN
        PRINT *,'MAxPUT .. This routine is not supported yet.'
        STOP
      ENDIF
#else
      IF(MODE.EQ.0) GO TO 3000
      IF(MODE.EQ.2) GO TO 4000
C
C ----------------------------------------------------------------------
C (2) MODE = 1 case.
C ----------------------------------------------------------------------
C
      DO 200 IK = 1, NKEY
        KEYTMP(:MCxKYL) = KEY(IK)
        KEYTMP(MCxKYL+1:MCxKEY) = CMxDLM(:MCxKEY-MCxKYL)
        CALL UIDATE(NDATA(1,IK))
        CALL   TIME(NDATA(2,IK))
        WRITE(MCxDBS,ERR=220)
     >        KEYTMP(:MCxKEY),NW(IK),(NDATA(K1,IK),K1=1,NW(IK))
        GO TO 240
C
C (2.2) When Key already exists
C
220     CONTINUE
        LKEY   = LEN(KEY(IK))
        MESAGE = ' %Warning_MAxPUT.. Key '//KEY(IK)(1:LKEY)//
     >           ' Already exists. '
        CALL MAMSG(3, 1, MESAGE)
C
        KEYGET = KEY(IK)
        READ(MCxDBS,KEY=KEYGET(:MCxKYL) )
     >       KEYTMP(:MCxKEY),IW,(IBUF(K1),K1=1,IW)
        READ(KEYTMP(MCxKYL+1:MCxKEY),*) IOVRS
        IOVRS = IOVRS - 1
        WRITE(KEYTMP(MCxKYL+1:MCxKEY),FMT=CMxVRS) IOVRS
        CALL UIDATE(NDATA(1,IK))
        CALL   TIME(NDATA(2,IK))
        WRITE(MCxDBS)
     >        KEYTMP(:MCxKEY), NW(IK),(NDATA(K1,IK),K1=1,NW(IK))
        NRET = 1
C
C (2.3) Print out log message.
C
240     CONTINUE
        IF(MCxMSL.GT.10) THEN
          MESAGE = ' '//KEYTMP(:MCxKEY)//' saved.'
          CALL MAMSG(11,1,MESAGE(:8+MCxKEY))
        ENDIF
200   CONTINUE
      GO TO 9000
C
C ----------------------------------------------------------------------
C (3) MODE = 0 case.
C ----------------------------------------------------------------------
C
3000  CONTINUE
      DO 300 IK = 1, NKEY
        IVRS    = 0
        KEYGET  = KEY(IK)
        READ(MCxDBS,KEY=KEYGET(:MCxKYL),ERR=320)
     >       KEYTMP(:MCxKEY),IW,(IBUF(K1),K1=1,IW)
        READ(KEYTMP(MCxKYL+1:MCxKEY),*) IOVRS
        IVRS  = MCxVSM - IOVRS
CCC     PRINT *,' IK=',IK,'  Obtained KEY=',KEYTMP(:MCxKEY)
        GO TO 330
320     CONTINUE
        KEYTMP(:MCxKYL) = KEY(IK)
        KEYTMP(MCxKYL+1:MCxKYL+1) = ';'
330     CONTINUE
        IVRS  = IVRS + 1
        IOVRS = MCxVSM - IVRS
        WRITE(KEYTMP(MCxKYL+1:MCxKEY),FMT=CMxVRS) IOVRS
CC      PRINT *,' IOVRS=',IOVRS,' KEYTMP=',KEYTMP(:MCxKEY)
        CALL UIDATE(NDATA(1,IK))
        CALL   TIME(NDATA(2,IK))
        WRITE(MCxDBS)
     >        KEYTMP(:MCxKEY), NW(IK),(NDATA(K1,IK),K1=1,NW(IK))
C
        IF(MCxMSL.GT.10) THEN
          MESAGE = ' '//KEYTMP(:MCxKEY)//' saved.'
          CALL MAMSG(11, 1, MESAGE(:8+MCxKEY))
        ENDIF
300   CONTINUE
      GO TO 9000
C
C ----------------------------------------------------------------------
C (4) Use KEY as it is.
C ----------------------------------------------------------------------
C
4000  CONTINUE
C
      LKEY = LEN(KEY(1))
      IF( LKEY .NE. MCxKEY ) THEN
        MESAGE = ' %Warning_MAxPUT.. Key '//KEY(1)(1:LKEY)//
     >           ' length ne MCxKEY'
        CALL MAMSG(0, 1, MESAGE)
        STOP
      ENDIF
C
      DO 4200 IK = 1, NKEY
        CALL UIDATE(NDATA(1,IK))
        CALL   TIME(NDATA(2,IK))
        WRITE(MCxDBS,ERR=4220)
     >        KEY(IK)(:MCxKEY),NW(IK),(NDATA(K1,IK),K1=1,NW(IK))
        GO TO 4240
C
C (2.2) When Key already exists
C
4220    CONTINUE
        LKEY   = LEN(KEY(IK))
        MESAGE = ' %Warning_MAxPUT.. Key '//KEY(IK)(1:LKEY)//
     >           ' Already exists. not saved.'
        CALL MAMSG(1, 1, MESAGE)
        GO TO 4200
C
C (2.3) Print out log message.
C
4240    CONTINUE
        IF(MCxMSL.GT.10) THEN
          MESAGE = ' '//KEY(IK)(:MCxKEY)//' saved.'
          CALL MAMSG(11,1,MESAGE(:8+MCxKEY))
        ENDIF
4200  CONTINUE
      GO TO 9000
C
C ----------------------------------------------------------------------
C (9) Put time stamp of data modification.
C ----------------------------------------------------------------------
C
9000  CONTINUE
      CALL MAxSTP
C
#endif
      RETURN
      END
