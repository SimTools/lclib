C **********************************************************************
C*                                                                     *
C*=========================================================            *
C* Subroutine TBxSEL( IPSUB, IPBNK, IPELM, HWORDS, HPDATA)             *
C*=========================================================            *
C*(Purpose)                                                            *
C*   Save the Bank_element NE.                                         *
C*(Input)                                                              *
C*   IPSUB    : Address of the DSB: ID in DSB_Parameter_Table: IDSUB   *
C*   IPBNK    : Address of the Bank in Bank_Name_Table: BNKNAM,NWBNK   *
C*   IPELM    : Address of Element in the Element_Number_List: LISTEL  *
C*   HWORDS   : Word_Count of the element                              *
C*(Output)                                                             *
C*   HPDATA   : Return_Flag                                            *
C*     HPDATA<0 : Warning; Bank_Element was not saved.                 *
C*     HPDATA>0 : Normal Return. This case IPDATA gives Address in DB. *
C*(Used COMMON)                                                        *
C*     /TBSBNK/                                                        *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85     Version up   V 3.0             *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBxSEL( IPSUB, IPBNK, IPELM, HWORDS, HPDATA)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsmes.inc"
C
      LOGICAL TBxCKS
C
C  --------------------------------------------------------------
C    (1) Check Word_Count whether the old space is enough.
C        If it is enough, then return.
C  --------------------------------------------------------------
C
         HPDATA    = IADTBL(1,IPELM) + NWSUB(1,IPSUB)
         HWDSB     = NWELM(1,IPELM)
         IF( HWORDS .LE. HWDSB ) GO TO 1000
C
C  -------------------------------------------------------------------
C      (2) Check whether there is enough space for the new element
C  -------------------------------------------------------------------
C
         HSEXT     = NWBNK(2,IPBNK)
         IF( HSEXT .GT. 0 ) THEN
             HWRDS   = HWORDS - HWDSB
             IF( HWRDS .GT. 0 ) THEN
                 HEXTD   = HWRDS/HSEXT + 1
                 HWDSB   = HEXTD*HSEXT + HWDSB
             ENDIF
             HWDSB   = ((HWDSB + 1)/2)*2
         ELSE
             HWDSB   = ((HWORDS+1)/2)*2
         ENDIF
         INCRMT    = HWDSB - NWELM(1,IPELM)
 
         CALL TBxSPC( IPSUB, IPELM, HWDSB, IRET )
         IF( IRET .GT. 0 ) THEN
             NWSUB(3,IPSUB) = NWSUB(3,IPSUB) + INCRMT
             GO TO 1000
         ENDIF
C                                                     ======
         IF(.NOT.TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA )) THEN
              IADTBL(1,IPELM) = -1
              IDSUB (8,IPSUB) =  1
C
              CALL TBxGAB( IPSUB )
C                                                        ======
            IF(.NOT.TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA )) THEN
 
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
                    HWNEED = HWORDS-NWSUB(2,IPSUB)
                    WRITE(MESAGE(1),9001)
 9001               FORMAT(' Data Buffer is too small')
                    WRITE(MESAGE(2),9002) HWNEED,NWFREE
 9002               FORMAT(1X,'Need more ',I6,' I*2 words, but',
     .                        '  only ',I6,' I*2 words are reserved')
                    CALL TBxERR( 600, 'TBxSEL', 2)
                ELSEIF(.NOT.
     .                  TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA ))
     .                                                        THEN
                    CALL TBLSUB
                    WRITE(MESAGE(1),9001)
                    CALL TBxERR( 600, 'TBxSEL', 1)
 
                ENDIF
            ENDIF
         ENDIF
 
         NWSUB(3,IPSUB) = NWSUB(3,IPSUB) + HWDSB
C
C  ------------------------------------------------------------------
C      (3)  Save new Address and word_count in the address table
C  ------------------------------------------------------------------
C
 1000      IADTBL(1,IPELM)  = (HPDATA/2)*2 + 1 - NWSUB(1,IPSUB)
           NWELM(1,IPELM)   = HWDSB
           NWELM(2,IPELM)   = HWORDS
 
           IF( LINK(3,IPELM) .GE. 0 ) THEN
               IF( LINK(3,IPELM) .EQ. 0 ) THEN
                   NPOINT   =  HPDATA
                   NWORD    =  HWORDS
               ELSE
                   NPOINT   =  (HPDATA+1)/2
                   NWORD    =  (HWORDS+1)/2
               ENDIF
               CALL UVSET( IADTBL(2,IPELM), NPOINT )
               CALL UVSET( IADTBL(3,IPELM), NWORD  )
           ENDIF
 
         RETURN
         END
