C **********************************************************************
C*                                                                     *
C*=============================================================        *
C* Subroutine TBxCEL( IPSUB, IPBNK, IPELM, NE, HWORDS, HPDATA)         *
C*=============================================================        *
C*(Purpose)                                                            *
C*   Create the Bank_Element NE.                                       *
C*(Caution)                                                            *
C*   Before use this routine check whether the Bank_Element is there.  *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB: ID in DSB_Parameter_Table: IDSUB   *
C*   IPBNK    : Address of the Bank in Bank_Name_Table: BNKNAM,NWBNK   *
C*   NE       : Bank_Element_Number                                    *
C*   HWORDS   : Word_Count of the element                              *
C*(Output)                                                             *
C*   HPELM    : address of the element in the Link and address tables. *
C*   HPDATA   : Return_Flag                                            *
C*     HPDATA<0 : Warning; Bank_Element was not created.               *
C*     HPDATA>0 : Normal Return. This case IPDATA gives Address in DB. *
C*(Used COMMON)                                                        *
C*     /TBSBNK/    For detail information, see TBREAD.                 *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85     Version up  V 3.0              *
C*       A.Miyamoto     May 28 '90  Stop when Element table overflows  *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBxCEL( IPSUB, IPBNK, IPELM, NE, HWORDS, HPDATA)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
C
      LOGICAL TBxCKS
C
         HWDSB   = ( (HWORDS+1)/2 )*2
         IF( NWBNK(1,IPBNK) .GT. HWDSB ) HWDSB = NWBNK(1, IPBNK)
C
C  ---------------------------------------------------------------
C     If there is not enough space for the new element, then
C  ---------------------------------------------------------------
C                                                     ======
         IF(.NOT.TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA )) THEN
C
             IF( IDSUB(8,IPSUB) .NE. 0 ) THEN
C
C  -------  Gabage_Collection  ---------------------------------------
C
                 CALL TBxGAB( IPSUB )
C
C  -------  Check again whether there is enough space. ---------------
C                                                        ======
                 IF(TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA ))
     .                                                        GO TO 50
C
            ENDIF
 
            IF( IDSUB(12,IPSUB) .GT. 0 ) THEN
                HEXTD = 2*IDSUB(12,IPSUB)
                NEXTD = HWDSB/HEXTD + 1
                HWNEED= NEXTD*HEXTD + NWSUB(2,IPSUB)
            ELSE
                HWNEED  = NWSUB(2,IPSUB)+HWDSB
            ENDIF
 
            NWNEED  = (HWNEED + 1)/2
 
            CALL TBxDEX( IPSUB, NWNEED, IRRET)
            IF( IRRET .LT. 0 ) THEN
                HWNEED = HWNEED-NWSUB(2,IPSUB)
                WRITE(MESAGE(1),9005)
 9005           FORMAT(' Data Buffer is too small')
                WRITE(MESAGE(2),9000) HWNEED,NWFREE
 9000           FORMAT(' Need more ',I6,' I*2 words, but',
     .                    '  only ',I6,' I*2 words are reserved')
                CALL TBxERR( 600,'TBxCEL', 2)
            ENDIF
            IF(.NOT.TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA ))
     .                                                        THEN
                CALL TBLSUB
                WRITE(MESAGE(1),9005)
                CALL TBxERR( 600,'TBxCEL', 1)
 
            ENDIF
C
         ENDIF
C
C  ------------------------------------------------------------------
C                    If there is enough space, then
C  ------------------------------------------------------------------
C      Get the offset of the first element and the allocated number of
C     elements in the IPSUB-th DSB.
C
   50    IBEGIN            = IDSUB(5,IPSUB)
         MXADD             = IDSUB(6,IPSUB) + IBEGIN
         IBEGIN            = IBEGIN + 1
C
C  ----  Look for the first unused space in the Link_Table: LINK ---
C
         DO  100  IPELM = IBEGIN, MXADD
           IF( LINK(1,IPELM) .LT. 0 ) GO TO 200
  100    CONTINUE
C
C  ----- If there is no unused space already, then try to extend the
C        element link table etc.
 
         ELNO  = 1.1*(IDSUB(6,IPSUB)+1)
         NOEL  = ELNO
 
         CALL TBxEEX( IPSUB, NOEL, IRRET )
 
         IF( IRRET .LT. 0 ) THEN
             HPDATA          = -1
             WRITE(MESAGE(1),9010)
 9010        FORMAT(' Element table Overflow')
             CALL TBxERR( 600,'TBxCEL', 1)
C// DEBUG
C            WRITE(6,9000) (L,(LINK(K,L),K=1,2),L=IBEGIN,MXADD)
C9000        FORMAT(1X,'In TBxCEL:      Add   LINK(1,i)   LINK(2,i)',
C    .        (/1X,'        ',3I12))
             RETURN
         ENDIF
 
         IPELM  = MXADD + 1
         MXADD  = NOEL
 
C  ------   If the number of elements NOELMT > 0 then ----------------
C      Look for the place in the Link_Table, where the element is to
C     be inserted in.
 
  200    NOELMT            = KEYBNK(2,IPBNK)
         LINK(1,IPELM)     = NE
         LINK(2,IPELM)     = -1
         IF( NOELMT .GT. 0 ) THEN
             IADD1           = KEYBNK( 1,IPBNK)
             IADD0           = 0
             DO   300    I   = 1, NOELMT
C
               IF( NE .LT. LINK(1,IADD1) ) GO TO 400
C
               IF( I .LT. NOELMT ) THEN
                 IADD0       = IADD1
                 IADD1       = LINK(2,IADD1)
               ENDIF
  300        CONTINUE
C
C       If the element_number is the largest one in the Bank, then
C      append the number.
C
             LINK(2,IADD1)   = IPELM
             LINK(2,IPELM)   = -1
             GO TO 500
C
  400        IF( IADD0 .LE. 0 ) THEN
                 LINK(2,IPELM)   = IADD1
                 KEYBNK(1,IPBNK) = IPELM
             ELSE
                 LINK(2,IPELM)   = IADD1
                 LINK(2,IADD0)   = IPELM
             ENDIF
C
C  --- If the number of elements NOELMT =< 0, then  ------------------
C      then set Address pointer in the KEYBNK(1,IPBNK) = IPELM.
C
         ELSE
             KEYBNK(1,IPBNK) = IPELM
             NOELMT          = 0
         ENDIF
C
C  -----------------------------------------------------------------
C                Update several parameters.
C  -----------------------------------------------------------------
C
  500    KEYBNK(2,IPBNK)   = NOELMT + 1
         IADTBL(1,IPELM)   = (HPDATA/2)*2+1-NWSUB(1,IPSUB)
         IADTBL(2,IPELM)   = 0
         IADTBL(3,IPELM)   = 0
         NWELM(1,IPELM)    = HWDSB
         NWELM(2,IPELM)    = HWORDS
         LINK(3,IPELM)     = -1
         NWSUB(3,IPSUB)    = NWSUB(3,IPSUB) + HWDSB
         IDSUB(7,IPSUB)    = IDSUB(7,IPSUB) + 1
 
         IDSIZE(IPSUB)     = IDSIZE(IPSUB) + HWDSB
 
         RETURN
         END
