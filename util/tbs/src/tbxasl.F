C***********************************************************************
C*                                                                     *
C*=========================================================            *
C* Subroutine TBxASL( IPSUB, IPBNK, IPELM, HWORDS, HPDATA)             *
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
C*     /TBSBNK/, /TBSBUF/ and /TBSCTL/                                 *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     July 20 '85     Version up   V 3.0             *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBxASL( IPSUB, IPBNK, IPELM, HWORDS, HPDATA)
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsctl.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
C
      LOGICAL TBxCKS
C
C  --------------------------------------------------------------
C    (1) Check Word_Count whether the old space is enough.
C        If it is enough, then return.
C  --------------------------------------------------------------
C
         IFLG      = 0
C        HOFSET    = IADTBL(1,IPELM) + NWSUB(1,IPSUB) - 1
         IAD0      = IADTBL(1,IPELM) + NWSUB(1,IPSUB)
         HWOLD     = NWELM(2,IPELM)
C        HPDATA    = HOFSET + HWOLD + 1
         HPDATA    = IAD0   + HWOLD
         HWDSB     = NWELM(1,IPELM)
         HWDNEW    = 0
         IF( HWORDS .LE. HWDSB ) GO TO 1000
C
C  -------------------------------------------------------------------
C      (2) Check whether there is enough space for the new element
C  -------------------------------------------------------------------
C
         IFLG      = 1
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
 
         HWDNEW    = HWDSB - NWELM(1,IPELM)
C
C  --- Test whether there is free space just after the current element
C
           CALL TBxSPC( IPSUB,  IPELM, HWDSB, IRET )
C
           IF( IRET .GE. 0 ) GO TO 1000
C
C -------------------------------------------------------------------
C    Copy existing data to the free space if there is enough space.
C -------------------------------------------------------------------
C                                                =======
         IF(TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA ) ) THEN
 
            HPDATA   = (HPDATA/2)*2 +1
 
            CALL UVCPYH( HWOLD, HBUFF(IAD0), HBUFF(HPDATA) )
 
C The following statements are for the versions before V402.
C           HPDATA   = (HPDATA/2)*2 +1
C           NOFSET   = HPDATA - 1
C           DO 150 I = 1, HWOLD
C             HBUFF(NOFSET+I) = HBUFF(HOFSET+I)
C150        CONTINUE
            HXDATA   = HPDATA
 
            HPDATA   = HPDATA + HWOLD
 
C --------------------------------------------------------------------
C    Not enough space left in the DSB, so do garbage correction.
C --------------------------------------------------------------------
 
         ELSE
C
C --------  Save the existing data into Data Buffer's free space ----
C
            IFG  = 0
            IF( HWOLD .GT. 0 ) THEN
C
                IF( HWOLD .LE. 2*MAXCTL )  THEN
 
                    CALL UVCPYH( HWOLD, HBUFF(IAD0), HCNTL(1))
 
C                   The following statements are
C                   for the versions before V402.
C                   DO 200 I = 1, HWOLD
C                      HCNTL(I)  = HBUFF( HOFSET+I )
C 200               CONTINUE
                    IFG  = 1
                ELSEIF( HWOLD .LE. NWFREE) THEN
 
                    HBOFT = MAXSIZ - HWOLD
                    IAD1  = HBOFT + 1
                    CALL UVCPYH( HWOLD, HBUFF(IAD0), HBUFF(IAD1))
 
C                   The following statements are
C                   for the versions before V402.
C                   HBOFT = MAXSIZ - HWOLD
C                   DO 210 I = 1, HWOLD
C                      HBUFF(HBOFT+I)  = HBUFF( HOFSET+I )
C 210               CONTINUE
                    IFG  = 2
                ELSE
                    NEED = (HWOLD - NWFREE + 1)/2
                    WRITE(MESAGE(1),9205)
 9205               FORMAT(' Data Buffer is too small to apend.')
                    WRITE(MESAGE(2),9210) NEED
 9210               FORMAT('More',I8,' words(I*4) is required',
     .                        ' in the Data Buffer')
                    CALL TBxERR( 800,'TBxASL', 2)
                ENDIF
            ENDIF
C
            IOLDEL          = LINK(1,IPELM)
            IADTBL(1,IPELM) = -1
            LINK(1,IPELM)   = -1
            IDSUB(8,IPSUB)  =  1
C
            CALL TBxGAB( IPSUB )
            LINK(1,IPELM)   = IOLDEL
C                                                        ======
            IF(.NOT.TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA ))
     .          THEN
 
                IF( IDSUB(12,IPSUB) .GT. 0 ) THEN
                    HEXTD = 2*IDSUB(12,IPSUB)
                    NEXTD = HWDSB/HEXTD + 1
                    HWNEED= NEXTD*HEXTD + NWSUB(2,IPSUB)
                ELSE
                    HWNEED  = NWSUB(2,IPSUB)+HWDSB
                ENDIF
 
                IF( IFG .EQ. 2 ) THEN
                    HNET    = HWNEED - NWSUB(2,IPSUB)
                    HXOFT   = NWSUB(1,NOSUB)+NWSUB(2,NOSUB)+HNET
                    NEED    = (HXOFT - HBOFT + 1)/2
                    IF( NEED .GT. 0 ) THEN
                        WRITE(MESAGE(1),9205)
                        WRITE(MESAGE(2),9210) NEED
                        CALL TBxERR( 800,'TBxASL', 2)
                    ENDIF
                ENDIF
 
                NWNEED  = (HWNEED + 1)/2
                CALL TBxDEX( IPSUB, NWNEED, IRRET)
                IF( IRRET .LT. 0 ) THEN
                    HWNEED = HWORDS-NWSUB(2,IPSUB)
                    WRITE(MESAGE(1),9205)
                    WRITE(MESAGE(2),9000) HWNEED,NWFREE
 9000               FORMAT(' Need more ',I6,' I*2 words, but',
     .                        '  only ',I6,' I*2 words are reserved')
                    CALL TBxERR( 600,'TBxASL', 2 )
                ENDIF
                IF(.NOT.TBxCKS( IPSUB, MAXSUB, NWSUB, HWDSB, HPDATA ))
     .                                                        THEN
                    CALL TBLSUB
                    WRITE(MESAGE(1),9205)
                    CALL TBxERR( 800,'TBxASL', 1)
 
                ENDIF
            ENDIF
C
C  ------------------------------------------------------------------
C       Recover the old data for the case HWOLD > 0
C  ------------------------------------------------------------------
C
            HXDATA       = HPDATA
            IF( HWOLD .GT. 0 ) THEN
               HOFSET    = HPDATA - 1
               IF( IFG .EQ. 1 ) THEN
 
                    CALL UVCPYH( HWOLD, HCNTL(1), HBUFF(HPDATA))
 
C                   The following statements are
C                   for the versions before V402.
C                  DO 400 I  = 1, HWOLD
C                     HBUFF( HOFSET+I ) = HCNTL( I )
C 400              CONTINUE
               ELSEIF( IFG .EQ. 2 ) THEN
 
                    CALL UVCPYH( HWOLD, HBUFF(IAD1), HBUFF(HPDATA))
 
C                   The following statements are
C                   for the versions before V402.
C                  DO 410 I  = 1, HWOLD
C                     HBUFF( HOFSET+I ) = HBUFF( HBOFT+I )
C 410              CONTINUE
               ENDIF
               HPDATA    = HPDATA + HWOLD
            ENDIF
C
         ENDIF
C  ------------------------------------------------------------------
C      (3)  Save new Address and word_count in the address table
C  ------------------------------------------------------------------
 
          IADTBL(1,IPELM)  = HXDATA - NWSUB(1,IPSUB)
          NWELM(1,IPELM)   = HWDSB
          NWELM(2,IPELM)   = HWORDS
          NWSUB(3,IPSUB)   = NWSUB(3,IPSUB) + HWDSB
 
          GO TO 2000
C
 1000     NWELM(1,IPELM)  = HWDSB
          NWELM(2,IPELM)  = HWORDS
          IF( IFLG .EQ. 1 ) THEN
 
             NWSUB(3,IPSUB)   = NWSUB(3,IPSUB) + HWDNEW
 
          ENDIF
          HXDATA            = IADTBL(1,IPELM) + NWSUB(1,IPSUB)
 
 2000     IF( LINK(3,IPELM) .GE. 0 ) THEN
              IF( LINK(3,IPELM) .EQ. 0 ) THEN
                  NPOINT   =  HXDATA
                  NWORD    =  HWORDS
              ELSE
                  NPOINT   =  (HXDATA+1)/2
                  NWORD    =  (HWORDS+1)/2
              ENDIF
              CALL UVSET( IADTBL(2,IPELM), NPOINT )
              CALL UVSET( IADTBL(3,IPELM), NWORD  )
          ENDIF
 
          IDSIZE(IPSUB)    = IDSIZE(IPSUB) + HWDNEW
 
        RETURN
        END
