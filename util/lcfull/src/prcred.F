C***********************************************************************
CC*********************************************************************C
C*
C*-------------------=======
C* Subroutine PRCRED(CMDNAM)
C*-------------------=======
C*
C*(Purpose)
C*    Process command for production and reduction job.
C*
C*(Input)
C*    CMDNAM ; Command name.
C*
C*(Author)
C*    A. Miyamoto   30-Apr-1987   Modified SSCRED..
C*    A. Miyamoto   13-Feb-1990   Use PRDBNK instead of SSDBNK
C*
CC**********************************************************************
C
      SUBROUTINE PRCRED(CMDNAM)
C
#include "prfctl.inc"
#include "prjctl.inc"
#include "prcnsv.inc"
#include "prjpr1.inc"
C
      CHARACTER*(*) CMDNAM
      CHARACTER     BNKNAM*64, RECNAM*32
      CHARACTER     BNKREM*64, RECREM*32
      CHARACTER     BNKDEL*64, RECDEL*32, CDEV*8
      DATA    BNKNAM/' '/, RECNAM/'EVENT'/
      DATA    BNKDEL/' '/, RECDEL/'EVENT'/,CDEV/' '/
      DATA    BNKREM/' '/, RECREM/'EVENT'/
C
C=======< Entry Point >=================================================
C
C (1) Define operand for the command 'CONSTANT at the initial call.
C
      LCMD = LEN(CMDNAM)
C
      IF(CMDNAM(1:1).NE.']') GO TO 200
C
      IF(CMDNAM(:LCMD).EQ.']OUTPUT') THEN
        CALL DBCOPR('OUTPUT','BANK',    'C*64',BNKNAM,IRET)
        CALL DBCOPR('OUTPUT','RECORD',  'C*32',RECNAM,IRET)
      ELSEIF(CMDNAM(:LCMD).EQ.']DELETE') THEN
        CALL DBCOPR('DELETE','BANK',    'C*64',BNKDEL,IRET)
        CALL DBCOPR('DELETE','RECORD',  'C*32',RECDEL,IRET)
      ELSEIF(CMDNAM(:LCMD).EQ.']REMOVE') THEN
        CALL DBCOPR('REMOVE','BANK',    'C*64',BNKREM,IRET)
        CALL DBCOPR('REMOVE','RECORD',  'C*32',RECREM,IRET)
 
      ELSEIF(CMDNAM(:LCMD).EQ.']PARAMETER') THEN
        CALL DBCOPR('PARAMETER','KILL_LEVEL',     'I*4 ',KLLVLR,IRET)
        CALL DBCOPR('PARAMETER','MAX_EVENT',      'I*4 ',NEVMAX,IRET)
        CALL DBCOPR('PARAMETER','DEBUG_FREQUENCY','I*4 ',NDFREQ,IRET)
        CALL DBCOPR('PARAMETER','INFILE', 'I*4', NINFIL,  IRET)
        CALL DBCOPR('PARAMETER','OUTFILE','I*4', NOTFIL,  IRET)
        CALL DBCOPR('PARAMETER','SIZE',   'R*4', XOSIZE,  IRET)
        CALL DBCOPR('PARAMETER','DEVICE', 'C*8', CDEV,    IRET)
        CALL DBCOPR('PARAMETER','INUNIT', 'I*4', NINUNI,  IRET)
        CALL DBCOPR('PARAMETER','OUTUNIT','I*4', NOTUNI,  IRET)
        CALL DBCOPR('PARAMETER','TIME_LEFT','R*4 ',RTIMLM,IRET)
        CALL DBCOPR('PARAMETER','RUN_TIME' ,'R*4 ',RCPUMX,IRET)
        CALL DBCOPR('PARAMETER','PRINT_CONSTANT' ,'I*4 ',NPRTCN,IRET)
        CALL DBCOPR('PARAMETER','DELETE_BANK' ,'F*4 ',NDELBK,IRET)
        CALL DBCOPR('PARAMETER','WRITE_ALL_RECORDS','F*4 ',NALLWT,IRET)
 
      ENDIF
      GO TO 900
C
C ----------------------------------------------------------------------
C (2) Process input command.
C ----------------------------------------------------------------------
C
200   CONTINUE
C
C (2.2) Output command.
C
      IF(CMDNAM(:LCMD).EQ.'OUTPUT') THEN
        CALL PROBNK(-1,BNKNAM,RECNAM)
C
C (2.3) Delete command.
C
      ELSEIF(CMDNAM(:LCMD).EQ.'DELETE') THEN
C900213    CALL SSDBNK(-1,BNKDEL,RECDEL)
        CALL PRDBNK(-1,BNKDEL,RECDEL)
C
C (2.3) Remove command.
C
      ELSEIF(CMDNAM(:LCMD).EQ.'REMOVE') THEN
        CALL PRRBNK(-1,BNKREM,RECREM)
C
C (2.3) Parameter command.
C
      ELSEIF(CMDNAM(:LCMD).EQ.'PARAMETER') THEN
        IF(CDEV(1:4).EQ.'DISK') THEN
           NDEVTP = 0
        ELSEIF(CDEV(1:3).EQ.'CTL') THEN
           NDEVTP = 1
        ENDIF
 
      ENDIF
 
C
C (9) Normal return
C
900   CONTINUE
      RETURN
      END
