C **********************************************************************
C*                                                                     *
C*======================================================               *
C* Subroutine TBPUT( ID, BNAME, NE, NWORDS, IDATA, IRET)               *
C*======================================================               *
C*(Purpose)                                                            *
C*   Put the NWORDS Data: IDATA into the Bank_Element BNAME:NE.        *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB                                   *
C*   BNAME    : Bank_Name                                              *
C*   NE       : Element_Number                                         *
C*   IDATA    : Data                                                   *
C*   NWORDS   : Word_Count of the input data                           *
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
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBPUT( ID, BNAME, NE, NWORDS, IDATA, IRET)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
      INTEGER       IDATA(NWORDS)
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
 
      HWORDS   = 2*NWORDS
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
C                     Find element NE
C -----------------------------------------------------------------
C                                =====  ====
         CALL TBxFEL( IPBNK, NE, IPELM, IRET)
C                                                           //// DEBUG
C                                    WRITE(6,9000) IPBNK,NE,IPELM,IRET
C9000                      FORMAT(1X,'TBPUT IPBNK,NE,IPELM,IRET=',4I6)
C
         IF( IRET .LT. 0 ) THEN
C                                       ======             ======
             CALL TBxCEL( IPSUB, IPBNK, IPELM, NE, HWORDS, HPDATA)
C
             IRET = 1
C
         ELSE
C                                 If IRET >=0, i.e. the Bank_Element is
C                                 found, then update
C                                 the Element_Pointer_Table: NPNTBL
C                                 and the Address_Table: IADTBL.
C                                                      ======
             CALL TBxSEL( IPSUB, IPBNK, IPELM, HWORDS, HPDATA)
C
             IRET = 2
         ENDIF
C
C  ------------------------------------------------------------------
C              Store the Data into IPSUB-th DSB
C  ------------------------------------------------------------------
 
         IF( NWORDS .GT. 0 ) THEN
 
           IOFSET            = (HPDATA+1)/2
 
           CALL UVCOPY( NWORDS, IDATA(1), IBUFF(IOFSET))
 
C The following statements are for the versions before V402.
C          IOFSET            = (HPDATA+1)/2 - 1
C          DO     500     I  = 1, NWORDS
C            IBUFF(I+IOFSET) = IDATA(I)
C 500      CONTINUE
 
         ENDIF
C
         RETURN
         END
