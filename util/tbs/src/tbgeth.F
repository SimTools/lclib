C **********************************************************************
C*                                                                     *
C*=================================================================    *
C* Subroutine TBGETH( ID, BNAME, NE, HWORDS, HDATA, IRET, /MLIMSZ/)    *
C*=================================================================    *
C*(Purpose)                                                            *
C*   Get the Data IDATA from the Bank_Element BNAME:NE.                *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   NE       : Element_Number                                         *
C*  /MLIMSZ/  : Size of aray IDATA.(Optional)                          *
C*(Output)                                                             *
C*   HDATA    : I*2 Data                                               *
C*   HWORDS   : Word_Count in I*2 unit                                 *
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
C*       S.Kawabata     July 20 '85                                    *
C*       S.Kawabata     Nov. 27 '85     Version V4.0                   *
C*       A.Miyamoto     Mar.20 '87     Add MLIMSZ operand.             *
C*                                                                     *
C **********************************************************************
 
       SUBROUTINE TBGETH( ID, BNAME, NE, HWORDS,
#ifdef  MSP
     >     HDATA, IRET, /MLIMSZ/)
#endif
#ifdef  UNIX
     >     HDATA, IRET )
#endif
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
      INTEGER*2     HDATA(*)
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
 
#ifdef  UNIX
      NUMARG = 6
#endif
#ifdef  UNIX
      GO TO 9900
#endif
#ifdef  UNIX
      ENTRY TBGETHx(ID, BNAME, NE, HWORDS,
#endif
#ifdef  UNIX
     >     HDATA, IRET, MLIMSZ)
#endif
#ifdef  UNIX
      NUMARG = 7
#endif
#ifdef  UNIX
9900  CONTINUE
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
           CALL TBxERR( 200,'TBGETH', 1)
 
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
             CALL TBxERR( 200,'TBGETH', 2)
 
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
             CALL TBxERR( 200,'TBGETH', 3)
 
           IRET = -3
 
           RETURN
         ENDIF
C
C  --------------------------------------------------------------------
C         Get the Ofset and word count of the Bank_Element
C  --------------------------------------------------------------------
 
         HOFSET  = IADTBL(1,IPELM) + NWSUB(1,IPSUB)
         HWORDS  = NWELM(2,IPELM)
 
         IW      = HWORDS
         IRET    = 1
         IF( NUMARG  .GE. 7 ) THEN
           IF( HWORDS .GT. MLIMSZ ) THEN
             IW   = MLIMSZ
             IRET = 2
           ENDIF
         ENDIF
 
         CALL UVCPYH( IW    , HBUFF(HOFSET), HDATA(1))
 
C
C        HOFSET  = IADTBL(1,IPELM) - 1 + NWSUB(1,IPSUB)
C        DO  500   I = 1, HWORDS
C           HDATA(I) = HBUFF( I + HOFSET)
C 500    CONTINUE
C
 
         RETURN
 
         END
