C **********************************************************************
C*                                                                     *
C*==================================================================   *
C* Subroutine TBLNKA( ID,BNAME,TYPE,ISIZE,NELIST,/LOC/,/NWRDS/,IRET)   *
C*==================================================================   *
C*(Purpose)                                                            *
C*   To link the location and word variables to bank elements.         *
C*   After this calling, location and # of words of the bank element   *
C*   are saved in LOC and NWORDS, respectively.                        *
C*   Even after the gabage collection, LOC and NWRDS  are updated      *
C*   as long as the record is on a Data subbuffer.                     *
C*(Usage)                                                              *
C*   PARAMETER (ISIZE = 160)                                           *
C*   COMMON /POINTR/ NELIST(ISIZE),LOC(ISIZE),NWRDS(ISIZE)             *
C*   CALL TBLINK(ID,'Bank_name','I*2',ISIZE,NELIST,LOC,NWRDS,IRET )    *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   TYPE     : Type attribution 'I*2' or 'I*4'                        *
C*   ISIZE    : Array size of the element list, location list and      *
C*              word count list.                                       *
C*   NELIST(ISIZE) : Element list                                      *
C*   /LOC(ISIZE)/  : Location list                                     *
C*   /NWRDS(ISIZE)/: Word count list                                   *
C*(Output)                                                             *
C*   LOC      : Location of the bank element in the Data Buffer        *
C*              by I*2 unit.                                           *
C*              I*2 (I*4).                                             *
C*   NWORDS   : Word_Count of the Bank element by I*2 unit.            *
C*   IRET     : Return_Flag                                            *
C*     IRET = 0 : No element                                           *
C*          =-1 : No bank                                              *
C*          =-NE: ISIZE is small                                       *
C*     IRET = NE: Normal return.                                       *
C*(Used COMMON)                                                        *
C*     /TBSBNK/ and /TBSBUF/  Further information, see TBREAD.         *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Nov.27 '85     Version V4.0                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLNKA( ID, BNAME,
#ifdef  MSP
     .             TYPE, ISIZE, NELIST, /LOC/, /NWRDS/, IRET)
#endif
#ifdef  UNIX
     .            TYPE, ISIZE, NELIST, LOC, NWRDS, IRET)
#endif
 
      IMPLICIT INTEGER*4 (H)
      INTEGER*4 NELIST(ISIZE)
 
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
      CHARACTER*(*) BNAME,TYPE
      CHARACTER*64  TNAME
      CHARACTER*3   I2,I4
      DATA I2,I4 / 'I*2', 'I*4'/
 
C  --------------------------------------------------------------------
C                    Check the several Parameters
C  --------------------------------------------------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBLINK', 1)
 
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
             CALL TBxERR( 200,'TBLNKA', 2)
 
           IRET = -1
           RETURN
         ENDIF
C
C  --------------------------------------------------------------------
C               Look for the Bank_Element BNAME; NE
C  --------------------------------------------------------------------
C
C  ---- Get the number of Bank_Elements: NBELMT in the Bank.  ---------
C
         NBELMT   = KEYBNK(2,IPBNK)
         IRET     = 0
C//// DEBUG
C        WRITE(6,9000) NBELMT
C9000    FORMAT(1X,'TBxFEL  NBELMT =',I6)
C
C  -- If the NBELMT is zero, i.e. still there is no Element, then ------
C  --   set IRET = 0  and return.                                 ------
C
         IF( NBELMT .LE. 0 ) RETURN
C----------------------------------------------------------------------
C     If the Bank has element, i.e. NBELMT > 0, then
C Loop over NBELMT times for filling element table etc of output.
C----------------------------------------------------------------------
      IF( NBELMT .GT. ISIZE ) NBELMT = ISIZE
 
      IADLOC   = NUADDR(   LOC ) - 4
      IADWRD   = NUADDR( NWRDS )- 4
 
      IPELM    = KEYBNK(1,IPBNK)
      DO 1000 I = 1, NBELMT
 
         NE         = LINK(1,IPELM)
         NELIST(I)  = NE
         IADLOC     = IADLOC + 4
         IADWRD     = IADWRD + 4
         IF( LINK(3,IPELM) .LT. 0 ) THEN
             LOC1   = IADTBL(1,IPELM) + NWSUB(1,IPSUB)
             NWORD1 = NWELM(2,IPELM)
             IADTBL(2,IPELM)  = IADLOC
             IADTBL(3,IPELM)  = IADWRD
             IF( TYPE .EQ. I4 ) THEN
                 NPOINT          =  (LOC1+1)/2
                 NWORD           =  (NWORD1+1)/2
                 LINK(3,IPELM)   = 1
             ELSE
                 NPOINT          =  LOC1
                 NWORD           =  NWORD1
                 LINK(3,IPELM)   = 0
                 IF( TYPE .NE. I2 ) THEN
                     WRITE(MESAGE(1),9031) TYPE
 9031                FORMAT(' ',A3,' is illegal ATT parameter.')
                     WRITE(MESAGE(2),9032)
 9032                FORMAT(' Assuming ATT = I*2')
                     CALL TBxERR( 200,'TBLINK', 2)
                     IRET  = -1
                 ENDIF
             ENDIF
         ELSE
             WRITE(MESAGE(1),9041)
 9041        FORMAT(' The following bank element is already linked.')
             WRITE(MESAGE(2),9042) ID, NE
 9042        FORMAT('  RECORD ID =',I4,'  Element# =',I4)
             WRITE(MESAGE(3),9022)  BNKNAM(IPBNK)
 9022        FORMAT(' Bank name = ',A64)
             CALL TBxERR( 200,'TBLINK', 3)
             NPOINT          =  -1
             NWORD           =  -1
             IRET  = -1
         ENDIF
         CALL UVSET( IADLOC, NPOINT )
         CALL UVSET( IADWRD, NWORD  )
         IPELM  = LINK(2,IPELM)
         IF( IPELM .LE. 0 ) GO TO 2000
 1000 CONTINUE
 
      IRET   = -NBELMT
      RETURN
 
 2000 IRET   = I
      RETURN
      END
