C **********************************************************************
C*                                                                     *
C*======================================================               *
C* Subroutine TBGET( ID, BNAME, NE, NWORDS, IDATA, IRET, /MLIMSZ/)     *
C*======================================================               *
C*(Purpose)                                                            *
C*   Get the Data IDATA from the Bank_Element BNAME:NE.                *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   NE       : Element_Number                                         *
C*  /MLIMSZ/  : Size of aray IDATA.(Optional)                          *
C*(Output)                                                             *
C*   IDATA    : Data                                                   *
C*   NWORDS   : Word_Count of the output Data, equal to the actual data*
C*             size in the bank element, even though data is trancated.*
C*   IRET     : Return_Flag                                            *
C*     IRET < 0 : Warning;                                             *
C*          =-1 : Warning; DSB ID does not exist.                      *
C*          =-2 : Warning; Bank does not exist.                        *
C*          =-3 : Warning; Bank element does not exist.                *
C*     IRET = 1 : Normal return.                                       *
C*          = 2 : When data is trancated.                              *
C*(Used COMMON)                                                        *
C*     /TBSBNK/ and /TBSBUF/  Further information, see TBREAD.         *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Nov.27 '85     Version V4.0                    *
C*       A.Miyamoto     Mar.20 '87     Add MLIMSZ operand.             *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBGET( ID, BNAME, NE, NWORDS,
#ifdef  MSP
     >     IDATA, IRET, /MLIMSZ/)
#endif
#ifdef  UNIX
     >     IDATA, IRET )
#endif
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
      INTEGER       IDATA(*)
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
 
#ifdef  UNIX
      NUMARG=6
#endif
#ifdef  UNIX
      GO TO 9000
#endif
#ifdef  UNIX
      ENTRY TBGETx(ID, BNAME, NE, NWORDS, IDATA, IRET, MLIMSZ)
#endif
#ifdef  UNIX
      NUMARG = 7
#endif
#ifdef  UNIX
9000  CONTINUE
#endif
#ifdef  MSP
         NUMARG = NARG(0)
#endif
C  --------------------------------------------------------------------
C                    Check the several Parameters
C  --------------------------------------------------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBGET', 1)
 
           IRET     = -1
           RETURN
         ENDIF
C
C  --------------------------------------------------------------------
C                    Find Bank_name: BNAME
C  --------------------------------------------------------------------
 
         TNAME = BNAME
 
         CALL UCUPER( LEN(BNAME), TNAME)
 
C                                   ====  =====  ====
         CALL TBxFBK( IPSUB, TNAME, ISRT, IPBNK, IRET)
C
         IF( IRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9011)
 9011        FORMAT(' The following bank does not exist.')
             WRITE(MESAGE(2),9012) TNAME
 9012        FORMAT(' ',A64)
             CALL TBxERR( 200,'TBGET', 2)
 
           IRET = -2
           RETURN
         ENDIF
C
C  --------------------------------------------------------------------
C               Look for the Bank_Element BNAME; NE
C  --------------------------------------------------------------------
C                                =====  ====
         CALL TBxFEL( IPBNK, NE, IPELM, IRET)
C
         IF( IRET .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9021)
 9021        FORMAT(' The following bank element does not exist.')
             WRITE(MESAGE(2),9022) TNAME
 9022        FORMAT(' Bank name = ',A64)
             WRITE(MESAGE(3),9023) NE
 9023        FORMAT(' Element # = ',I6)
             CALL TBxERR( 200,'TBGET', 3)
 
           IRET = -3
 
           RETURN
         ENDIF
C
C  --------------------------------------------------------------------
C         Get the Ofset and word count of the Bank_Element
C  --------------------------------------------------------------------
 
         IOFSET  = (IADTBL(1,IPELM)+NWSUB(1,IPSUB)+1)/2
         HWORDS  = NWELM(2,IPELM)
         NWORDS  = (HWORDS+1)/2
 
         IW      = NWORDS
         IRET    = 1
         IF( NUMARG .GE. 7 ) THEN
           IF( NWORDS .GT. MLIMSZ ) THEN
             IW   = MLIMSZ
             IRET = 2
           ENDIF
         ENDIF
 
         CALL UVCOPY( IW    , IBUFF(IOFSET), IDATA(1))
 
C The following statements are for the versions before V402
C        IOFSET  = (IADTBL(1,IPELM)+NWSUB(1,IPSUB)+1)/2 - 1
C        DO  500   I = 1, NWORDS
C           IDATA(I) = IBUFF( I + IOFSET)
C 500    CONTINUE
 
 
         RETURN
C
         END
