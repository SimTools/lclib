C **********************************************************************
C*                                                                     *
C*======================================================               *
C* Subroutine TBQPT( ID, BNAME, NOELM,
C*                          NE, NWORDS, IDATA, IRET )                  *
C*======================================================               *
C*(Purpose)                                                            *
C*   Quick Multiple-put routine.                                       *
C*   Put data of all elements in to the bank
C*   !! Warning !!                                                     *
C*      This routine puts new elements. If same element exists,        *
C*      results is unpredictable.                                      *
C*      No gabage correction and element table extentsion.             *
C*      No element should be exists before call to this routine.       *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   NOELM    : Number of elements.                                    *
C*   NE(NOELM): Element_Number                                         *
C*      Element # should be sorted from small number to larger number
C*   NWORDS(NOELM) : Word ccunt of the input data.                     *
C*   IDATA(*)      : Data                                              *
C*                   Data should be packed sequentially so that
C*                   all data can be copied to TBSBUF with a single
C*                   copy.
C*     First element data  IDATA(1) --> IDATA(NWORDS(1))
C*     2nd element data    IDATA(NWORDS(1)+1) -->
C*                                      IDATA(NWORDS(1)+NWORDS(2))
C*   ......
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*          -1  : Warning; DSB ID does not exist.                      *
C*          -2  : Warning; Bank BNAME does not exist.                  *
C*          -3  : Warning; Bank is protected.                          *
C*     IRET >=0 : Normal return                                        *
C*           1  : Normal Return;  The element was replaced.            *
C*           2  : Normal Return;  New element was created.             *
C*(Used COMMON)                                                        *
C*     /TBSBNK/ and /TBSBUF/  For further information, see TBREAD.     *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85     Version V 3.0                  *
C*       S.Kawabata     Nov. 27 '85     Version V 4.0                  *
C*       A.Miyamoto     Mar. 31 '93                                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBQPT( ID, BNAME, NOELM,
     >                      NE, NWORDS, IDATA, IRET)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
      INTEGER       NE(NOELM), NWORDS(NOELM), IDATA(*)
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
 
C --------------------------------------------------------------------
C                 Check the several Parameters
C --------------------------------------------------------------------
C                         =====
      CALL TBxFRD( ID, IPSUB )
C
      IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBPUT', 1)
 
           IRET     = -1
           RETURN
      ENDIF
C
C --------------------------------------------------------------------
C                  Find Bank_name: BNAME
C --------------------------------------------------------------------
 
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
             CALL TBxERR( 200,'TBPUT', 2)
 
           IRET = -2
           RETURN
      ENDIF
C -----------------------------------------------------------------
C      Check whether is there enough space to put all data.
C -----------------------------------------------------------------
C
      NWDSUM = 0
      DO 100 IE = 1, NOELM
        NWDSUM = NWDSUM + NWORDS(IE)
100   CONTINUE
      NXADD  = NWSUB(1,IPSUB)
      HPDATA = NXADD + NWSUB(3,IPSUB) + 1
      IOFST0 = (HPDATA+1)/2
      NXADD  = NXADD + NWSUB(2,IPSUB)
      IF( HPDATA+NWDSUM-1 .GT. NXADD ) THEN
         CALL TBLSUB
         HWNEED = HWNEED-NWSUB(2,IPSUB)
         WRITE(MESAGE(1),9005)
 9005    FORMAT(' Data Buffer is too small')
         WRITE(MESAGE(2),9000) HWNEED,NWFREE
 9000    FORMAT(' Need more ',I6,' I*2 words, but',
     .                    '  only ',I6,' I*2 words are reserved')
         CALL TBxERR( 600,'TBMPT ', 2)
         CALL ERRTRA
         STOP
       ENDIF
C
C -----------------------------------------------------------------
C      There is enough room to put all data.
C -----------------------------------------------------------------
C      Get the offset of the first element and the allocated number of
C     elements in the IPSUB-th DSB.
C
   50 CONTINUE
C
C  ----  Check number of elements in this bank. --------------------
C
      NBELMT = KEYBNK(2,IPBNK)
      IF( NBELMT .GT. 0 ) THEN
         WRITE(MESAGE(1),9205) NBELMT
 9205    FORMAT(' There are ',I5,
     .            ' Data element already exists on this bank')
         WRITE(MESAGE(2),9200) TNAME
 9200    FORMAT(' Bank name is ',A)
         CALL TBxERR( 600,'TBMPT ', 2)
         CALL ERRTRA
         STOP
       ENDIF
C
C ====================================================
C  Look for the space to put bank element.
C ====================================================
C
      IBEGIN            = IDSUB(5,IPSUB)
      MXADD             = IDSUB(6,IPSUB) + IBEGIN
      IBEGIN            = IBEGIN + 1
      IOLDNE            = 0
 
C ... Look for space in LINK table.
       DO  200  IPELM = IBEGIN, MXADD
           IF( LINK(1,IPELM) .LT. 0 ) GO TO 300
  200  CONTINUE
C
C  ----- If there is no unused space already, then try to extend the
C        element link table etc.
 
         WRITE(MESAGE(1),9010)
 9010    FORMAT(' Element table Overflow')
         CALL TBxERR( 600,'TBMPT ', 1)
         CALL ERRTRA
         STOP
C
C=============================================================
C ... Make a loop by a number of elements.
C=============================================================
C
300   CONTINUE
      KEYBNK(1,IPBNK) = IPELM
      IPOLDE = -1
      DO 1000 IE = 1, NOELM
        IF( IOLDNE.GT.NE(IE) ) THEN
          WRITE(MESAGE(1),9070) IOLDNE, NE(IE)
9070      FORMAT(' Element # is not sorted.  IOLDNE=',I4,
     >           '  NE =',I4)
          WRITE(MESAGE(2),9071) TNAME
9071      FORMAT(' Bank name was ',A)
          CALL TBxERR( 600,'TBMPT ', 2)
          CALL ERRTRA
          STOP
        ENDIF
        IOLDNE = NE(IE)
C
        DO 1200  IPELM = IBEGIN, MXADD
           IF( LINK(1,IPELM) .LT. 0 ) GO TO 1300
 1200   CONTINUE
C
C  ----- If there is no unused space already, then try to extend the
C        element link table etc.
 
         WRITE(MESAGE(1),9010)
         CALL TBxERR( 600,'TBMPT ', 1)
         CALL ERRTRA
         STOP
 
C  ------   If the number of elements NBELMT > 0 then ----------------
C      Look for the place in the Link_Table, where the element is to
C     be inserted in.
 
 1300    CONTINUE
         LINK(1,IPELM)     = NE(IE)
         IF( IPOLDE.GT.0 ) LINK(2,IPOLDE) = IPELM
         LINK(3,IPELM)     = -1
         IPOLDE = IPELM
         IBEGIN = IBEGIN + 1
C
C  -----------------------------------------------------------------
C                Update several parameters.
C  -----------------------------------------------------------------
C
         HWORDS = 2*NWORDS(IE)
         IADTBL(1,IPELM)   = (HPDATA/2)*2+1-NWSUB(1,IPSUB)
         IADTBL(2,IPELM)   = 0
         IADTBL(3,IPELM)   = 0
         NWELM(1,IPELM)    = HWORDS
         NWELM(2,IPELM)    = HWORDS
 
         IOFSET = (HPDATA+1)/2
         HPDATA = HPDATA + HWORDS
1000  CONTINUE
      CALL UVCOPY(NWDSUM,IDATA(1),IBUFF(IOFST0))
      LINK(2,IPELM)   = -1
      KEYBNK(2,IPBNK) = NOELM
      NWSUB(3,IPSUB)  = NWSUB(3,IPSUB) + 2*NWDSUM
      IDSUB(7,IPSUB)  = IDSUB(7,IPSUB) + NOELM
      IDSIZE(IPSUB)   = IDSIZE(IPSUB) + 2*NWDSUM
 
      IRET = 2
 
      RETURN
      END
