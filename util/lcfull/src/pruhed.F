C***********************************************************************
C*                                                                     *
C* --------------------------------                                    *
C*  Subroutine PRUHED(IDREC)                                           *
C* --------------------------------                                    *
C*                                                                     *
C*(Function)                                                           *
C*    Unpack data header block.                                        *
C*                                                                     *
C*(Input)                                                              *
C*    IDREC  : Record ID                                               *
C*                                                                     *
C*(Author)                                                             *
C*    A. Miyamoto    27-Apr-1987  Original version.                    *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE PRUHED( IDREC )
C
#include "prhead.inc"
#include "prgenf.inc"
C
#ifdef  UNIX
      INTEGER*4 IHEAD(20)
#endif
      DATA  MXWORD/18/
C
C===============< Entry Point >=================================
C
C ----------------------------------------------------------------------
C (1) Unpack Header block.
C ----------------------------------------------------------------------
C
C
      IF( IDREC .EQ. 2) THEN
#ifdef  MSP
        CALL TBGET(IDREC,'Begin_Run:Universal_Record_Header',1,
#endif
#ifdef  UNIX
        CALL TBGETx(IDREC,'Begin_Run:Universal_Record_Header',1,
#endif
     >                   NW, xTWORD, IRET, MXWORD)
      ELSEIF( IDREC .EQ. 8 ) THEN
#ifdef  MSP
        CALL TBGET(IDREC,'End_Run:Universal_Record_Header',1,
#endif
#ifdef  UNIX
        CALL TBGETx(IDREC,'End_Run:Universal_Record_Header',1,
#endif
     >                   NW, xTWORD, IRET, MXWORD)
      ELSE
#ifdef  MSP
     CALL TBGET(1,'Raw_Data:Header', 1, NW, xTWORD, IRET, MXWORD)
#endif
#ifdef  UNIX
        CALL TBGETx(1,'Raw_Data:Header',1,NW,xTWORD,IRET,MXWORD)
#endif
      ENDIF
      IF( IRET .GE. 0 ) RETURN

C
200   CONTINUE
      IF( NUMGEN .NE. 0 ) THEN
        CALL UVZERO( MXWORD, xTWORD)
        xRECTP = IDREC
        xEVTNO = 1
        xRUNTP = 1
        RETURN
      ENDIF

      CALL UVZERO(MXWORD, xTWORD)
      CALL TBGET(1,'Generator:Header',1,NW,IHEAD,IRET)
      IF( IRET.GT.0) THEN
        xEVTNO = IHEAD(1)
      ENDIF

      xRECTP = IDREC
      xEXPNO = 1
      xRUNNO = 1
C
      RETURN
      END
