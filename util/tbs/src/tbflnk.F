C **********************************************************************
C*                                                                     *
C*==================================                                   *
C* Subroutine TBFLNK( ID,BNAME,IRET)                                   *
C*==================================                                   *
C*(Purpose)                                                            *
C*   Free Link table of all elements of the bank.                      *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET = 0 : No element                                           *
C*          =-1 : No bank                                              *
C*     IRET = NE: Normal return.                                       *
C*(Used COMMON)                                                        *
C*     /TBSBNK/ and /TBSBUF/  Further information, see TBREAD.         *
C*(Author)                                                             *
C*     A. Miyamoto  May 1, '87  Modified from TBLNKA                   *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBFLNK( ID, BNAME, IRET)
 
      IMPLICIT INTEGER*4 (H)
 
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
 
C  --------------------------------------------------------------------
C                    Check the several Parameters
C  --------------------------------------------------------------------
C                         =====
         CALL TBxFRD( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBFLNK', 1)
 
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
             CALL TBxERR( 200,'TBFLNK', 2)
 
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
C
C  -- If the NBELMT is zero, i.e. still there is no Element, then ------
C  --   set IRET = 0  and return.                                 ------
C
         IF( NBELMT .LE. 0 ) RETURN
C----------------------------------------------------------------------
C     If the Bank has element, i.e. NBELMT > 0, then
C Loop over NBELMT times for filling element table etc of output.
C----------------------------------------------------------------------
 
C     IADLOC   = NUADDR(   LOC ) - 4
C     IADWRD   = NUADDR( NWRDS )- 4
 
      IPELM    = KEYBNK(1,IPBNK)
      DO 1000 I = 1, NBELMT
 
         IF( LINK(3,IPELM) .GE. 0 ) THEN
             LINK(3,IPELM) = -1
             IADTBL(2,IPELM) = 0
             IADTBL(3,IPELM) = 0
         ENDIF
         IPELM  = LINK(2,IPELM)
         IF( IPELM .LE. 0 ) GO TO 2000
 1000 CONTINUE
 
 
 2000 IRET   = I
      RETURN
      END
