C *********************************************************************C
C*                                                                     *
C*===============================                                      *
C* Subroutine TBLEL0(ID,BNAME,NE)                                      *
C*===============================                                      *
C*(Purpose)                                                            *
C*   Dump contents of TBS_Buffer.                                      *
C*(Input)                                                              *
C*   ID     : DSB ID.                                                  *
C*   BNAME  : Bank_name.                                               *
C*   NE     : Element no.                                              *
C*   MAXWRD : # of words to be listed, list all data when = 0.         *
C*(Used COMMON)                                                        *
C*  /TBSBNK/, /TBSBUF/                                                 *
C*(Author)                                                             *
C*       A.Miyamoto     June 13 '84                                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLEL0(ID,BNAME,NE,MAXWRD)
 
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsprm.inc"
#include "tbsmes.inc"
C
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
      CHARACTER*4   CHARA
 
C (1) Check bank_name length.
 
      LBNAM   = LEN(BNAME)
      IF(LBNAM.GT.64) THEN
 
           WRITE(MESAGE(1),9001) LBNAM
 9001      FORMAT(' The bank name of ',I3,' characters is too long.')
           WRITE(MESAGE(2),9002)
 9002      FORMAT(' The bank name is truncated by 64 characters.')
           LBNAM = 64
           WRITE(MESAGE(3),9003) BNAME(1:LBNAM)
 9003      FORMAT(' Bank name = "',A,'"')
           CALL TBxERR( 200,'TBLEL0', 3)
 
      END IF
 
C    1.1  Convert to the upper case character.
 
         TNAME = BNAME
 
         CALL UCUPER( LBNAM, TNAME)
 
C (2) Get IPSUB.. Pointer to Record ID
 
C     -----------------========
      CALL TBxFRD ( ID, IPSUB )
C     -----------------========
      IF ( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9011) ID
 9011      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBELE0', 1)
 
        RETURN
      ENDIF
 
C (3) Get IPBNK, and ISRT
 
C     ---------------------------====================
      CALL TBxFBK ( IPSUB, TNAME, ISRT, IPBNK, IRET )
C     ---------------------------====================
 
      IF ( IRET  .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9021) ID
 9021        FORMAT(' The following bank does not exist in the REC-ID ('
     .              ,I3,' )')
             WRITE(MESAGE(2),9022) TNAME
 9022        FORMAT(' Bank name = ',A64)
             CALL TBxERR( 200,'TBELE0', 2)
 
        RETURN
      ENDIF
 
C (4) Get IPELM, pointer to element.
 
C     ------------------------=============
      CALL TBxFEL ( IPBNK, NE, IPELM, IRET )
C     ------------------------=============
      IF ( IRET  .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9031) NE,ID
 9031        FORMAT(' Element#',I4,' does not exist in the REC-ID ('
     .              ,I3,' )')
             WRITE(MESAGE(2),9022) TNAME
             CALL TBxERR( 200,'TBELE0', 2)
 
        RETURN
      ENDIF
 
C (5) Write dump title.
 
      WRITE(LUNIT,300) SUBNAM(IPSUB), ID
300   FORMAT(//,' *** TBS dump bank element data ***',/,
     >       5X,' DSB_name  = ',A64,' DSB ID    =',I4)
 
      IF( IDSUB(11,IPSUB) .GT. 0 ) THEN
          IASH  = IDSUB(11,IPSUB)
          ISSH  = IDSUB(10,IPSUB)
          WRITE(LUNIT,302) SHRNAM(IASH,ISSH), IDSHR(IASH,ISSH)
302       FORMAT(5X,' REC_name  = ',A64,' REC ID    =',I4)
      ENDIF
 
      WRITE(LUNIT,305) BNKNAM(IPBNK), NE
305   FORMAT(5X,' Bank_name = ',A64,' Element # =',I4)
 
C (6) Get header part of element data,  and write them.
 
      IPDATA = (IADTBL(1,IPELM)+NWSUB(1,IPSUB)+1)/2
      ISPACE = (NWELM(1,IPELM)+1)/2
      NWORDS = (NWELM(2,IPELM)+1)/2
 
      WRITE(LUNIT,400) ISPACE, NWORDS
400   FORMAT(5X,' Temp. space =',I5,' #WORDS=',I8)
      WRITE(LUNIT,410)
      WRITE(LUNIT,430)
      WRITE(LUNIT,420)
      WRITE(LUNIT,430)
410   FORMAT('              |        E L E M E N T ',
     >       '    D A T A ')
420   FORMAT('  WRD#    Addr.      (I*4)    (I*2)',
     >       '  (I*2)       (R*4)      Hex.      (A*4)')
430   FORMAT(' ----|--------|------------|------|',
     >       '------|------------|---------|---------|')
C*
C*(7) Dump data.
C*
      IADD   = IPDATA -1
      MXWORD = MAXWRD
      IF( MXWORD .GT. NWORDS) MXWORD = NWORDS/2 + 1
      IF( MXWORD .LE. 0 )  MXWORD = NWORDS/2 + 1
      MINWRD = NWORDS - MXWORD
 
      IFG    = 0
      DO 500 IWRD = 1, NWORDS
      IADD   = IADD + 1
      IADD2  = 2*IADD
 
      IF( IWRD .GT. MXWORD .AND. IWRD .LT. MINWRD ) THEN
        IF( IFG .NE. 0 ) GO TO 500
           WRITE(LUNIT,450)
  450      FORMAT(10X,'.......................')
           IFG  = 1
        GO TO 500
      ENDIF
 
      CALL TBxITC( IBUFF(IADD), CHARA)
 
      WRITE(LUNIT,510) IWRD, IADD, IBUFF(IADD), HBUFF(IADD2-1),
     >           HBUFF(IADD2), RBUFF(IADD),    IBUFF(IADD), CHARA
510   FORMAT(1X,     I5,   I9,         I13,             I7,
     >                     I7,       G13.6,          2X,Z8,      2X,A4)
 
500   CONTINUE
 
      RETURN
      END
