C *********************************************************************C
C*                                                                     *
C*===============================                                      *
C* Subroutine TBREAD( LUN, IRET )                                      *
C*===============================                                      *
C*(Purpose)                                                            *
C*   Read Data from the Logical_unit LUN into the Data_Sub_Buffer      *
C*(Input)                                                              *
C*   LUN      : Logical_Unit_Number for the Input Device               *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET =     1 : Normal Return                                    *
C*     IRET = -1000 : EOF found                                        *
C*(Used COMMON)                                                        *
#include "ctlexp.inc"
#include "bnkexp.inc"
#include "bufexp.inc"
C*(Author)                                                             *
C*       S.Kawabata     June 2 '84                                     *
C*       S.Kawabata     July 16 '85  Version up V3.0                   *
C*       S.Kawabata     Jan. 13 '86  Version up V4.0                   *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBREAD( LUN, IRET )
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsctl.inc"
#include "tbspas.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
#include "tbsfmt.inc"
         CHARACTER*64 RECNAM
         LOGICAL      TBxRED,TBxSKP
         LOGICAL      TBREDF
 
C  =================================================================
C     Read Control_Block Data into the Buffer ICNTL
C  =================================================================
C ****                         ======  =====
 1000    CONTINUE
#ifdef  UNIX
         IF( MOD(NRWFMT(1),20).EQ.0 .OR.MOD(NRWFMT(1),20).EQ.4 
     >      .OR. NRWFMT(1).EQ.5 ) THEN
#endif
            TBREDF = TBxRED( LUN, NWCNTL, ICNTL )
#ifdef  UNIX
         ELSE
#endif
#ifdef  UNIX
            TBREDF = TBxRED( MAXCTL-1, NWCNTL, ICNTL )
#endif
#ifdef  UNIX
         ENDIF
#endif
C        print *,'in TBREAD ..TBREDF =',TBREDF
C        print *,'lun,nwcntl, icntl =',lun,nwcntl,
C     >  (icntl(k),k=1,20)
 
         IF( .NOT.TBREDF ) THEN
C ****
           NTREAD = NTREAD + 1
           NREDER = NREDER + 1
           IRET  = -1000
           RETURN
         ENDIF
         NTREAD = NTREAD + 1
 
         IF( ICNTL(2) .LT. 401 ) THEN
                      WRITE(MESAGE(1),9001) ICNTL(2)
 9001                 FORMAT('  Version # (',I5,' ) is too old.')
                      CALL TBxERR( 700,'TBREAD', 1)
         ENDIF
 
C  -----------  Get the DSB_Name from the Control_Block ------------
 
         HWORDS  = ICNTL(  1)
         NWRDS   = (HWORDS+1)/2
 
C        PRINT *,'LUN,ICNTL(1)-(3) =',LUN,ICNTL(1),ICNTL(2),ICNTL(3)
 
C                                    ======
#ifdef  UNIX
         IF( MOD(NRWFMT(1),20).EQ.0 .OR. MOD(NRWFMT(1),20).EQ.4) THEN
#endif
         CALL TBxCHA( ICNTL(4), 16,  RECNAM )
#ifdef  UNIX
         ELSE
#endif
#ifdef  UNIX
           CALL UVCOPY( 16, ICNTL(4), RECNAM )
#endif
#ifdef  UNIX
           CALL UCEBC(64, RECNAM )
#endif
#ifdef  UNIX
         ENDIF
#endif
C                                                            /// DEBUG
C                                                 WRITE(6,9000) RECNAM
C9000                                    FORMAT(1X,'DSB_NAME ',2X,A64)
C
C  ----------- Check whether Record Name RECNAM exist -----------------
C
         ISKIP = 0
         IF( NOSUB .GT. 0 ) THEN
C                               ====  =====
           CALL TBxCRN( RECNAM, IASH, IPSUB )
 
           IF( IPSUB .GT. 0 ) THEN
               IF( IASH .GT. 0 ) THEN
                   IDSUB(11,IPSUB) = IASH
                   ISSH            = IDSUB(10,IPSUB)
                   IDRET           = IDSHR(IASH, ISSH)
               ELSE
                   IDSUB(11,IPSUB) = 0
                   IDRET           = IDSUB(1,IPSUB)
               ENDIF
               GO TO 200
           ENDIF
         ENDIF
 
         ISKIP = 1
 
         IF( NOPAS .GT. 0 ) THEN
             DO 100 I = 1,NOPAS
                IF( RECNAM .EQ. PASNAM(I) ) THEN
                    ISKIP  = 2
                    LUOUT  = LUNPAS(I)
                    GO TO 200
                ENDIF
  100        CONTINUE
         ENDIF
 
C  =================================================================
C     If the Rec_Name exists, then unpack the Control_Block_Data
C  =================================================================
C       ****
  200    IF( ISKIP .EQ. 0 ) THEN
C       ****
C  --------- Test whether the size of input data match with -----------
C  --------- the size of the IPSUB-th Data_Sub_Buffer.      -----------
C  ---------   If the size is smaller than the input data, -----------
C  --------- then fatal error STOP.                         -----------
C
              IF( NWSUB(2,IPSUB) .LT. HWORDS ) THEN
                  NWNEED  = (HWORDS+1)/2
                  CALL TBxDEX( IPSUB, NWNEED, IRRET)
                  IF( IRRET .LT. 0 ) THEN
                      HWNEED = HWORDS-NWSUB(2,IPSUB)
                      WRITE(MESAGE(1),9002)
 9002                 FORMAT('  Data Buffer is too small')
                      WRITE(MESAGE(2),9003) HWNEED,NWFREE
 9003                 FORMAT('  Need more ',I6,' I*2 words, but',
     .                          '  only ',I6,' I*2 words are reserved')
                      CALL TBxERR( 600,'TBREAD', 2)
                  ENDIF
 
              ENDIF
 
C  ---------- Reset linked pointer and word parameters    -------------
 
              DO 300 I = IDSUB(5,IPSUB)+1,IDSUB(5,IPSUB)+IDSUB(7,IPSUB)
                 IF( LINK(3,I) .GE. 0 ) THEN
                     CALL UVSET( IADTBL(3,I), -1  )
                     LINK(3,I)   = -1
                     IADTBL(2,I) =  0
                 ENDIF
  300         CONTINUE
 
C  ---------- Initialize the DSB_Parameters for new data  -------------
 
              ID            = IDSUB(1,IPSUB)
C                              ====
              CALL TBxINI( ID, IRET )
 
C  ------------------ Get several Paramters ------------------------
C  i.e. Numbers of Banks and  Elements and Pointers to Bank_name etc.
 
              NOBANK  = HCNTL( 39)
              NOELMT  = HCNTL( 41)
              IOFSRT  = HCNTL( 43)
              IOFNAM  = HCNTL( 44)
              IOFNWB  = HCNTL( 45)
              IOFKEY  = HCNTL( 46)
              IOFLNK  = HCNTL( 47)
              IOFELM  = HCNTL( 48)
              IOFADT  = HCNTL( 49)
 
C  ------------------ Check numbers of banks and elements ----------
 
              IF( NOBANK .GT. IDSUB(3,IPSUB) ) THEN
 
                  CALL TBxBEX( IPSUB, NOBANK, IRRET)
                  IF( IRRET .LT. 0 ) THEN
 
                      WRITE(MESAGE(1),9011)
 9011                 FORMAT(' Too many Banks ')
                      WRITE(MESAGE(2),9012) MAXBNK
 9012                 FORMAT(' Max. number of banks =',I6)
                      CALL TBxERR( 700,'TBREAD', 2)
 
                  ENDIF
 
              ENDIF
              IF( NOELMT .GT. IDSUB(6,IPSUB) ) THEN
 
                  CALL TBxEEX( IPSUB, NOELMT, IRRET )
                  IF( IRRET .LT. 0 ) THEN
 
                      WRITE(MESAGE(1),9021)
 9021                 FORMAT(' Too many Bank elements ')
                      WRITE(MESAGE(2),9022) MAXELM
 9022                 FORMAT(' Max. number of elements =',I6)
                      CALL TBxERR( 700,'TBREAD', 2)
 
                  ENDIF
 
              ENDIF
 
C  --------- Copy to the Sort_Table: ISORT, Bank_Name_Table: ----------
C  ---------  BNKNAM and Bank_Key_Table: NWBNK and KEYBNK.  ----------
C
              IF( NOBANK .GT. 0 ) THEN
                IBOFT   = IDSUB(2,IPSUB)
                IEOFT   = IDSUB(5,IPSUB)
                IPBNAM  = IOFNAM
                DO 400 I= 1, NOBANK
 
                  IADB        = IBOFT + I
                  IOFSRT      = IOFSRT + 1
                  ISORT(IADB) = HCNTL(IOFSRT) + IBOFT
                  IPBNAM      = IPBNAM + 1
                  NWS         = ICNTL(IPBNAM)
C                                                     ============
#ifdef  UNIX
          BNKNAM(IADB)= ' '
#endif
#ifdef  UNIX
          IF( MOD(NRWFMT(1),20).EQ.0 .OR. MOD(NRWFMT(1),20).EQ.4) THEN
#endif
                  CALL TBxCHA( ICNTL(IPBNAM+1), NWS,  BNKNAM(IADB))
#ifdef  UNIX
          ELSE
#endif
#ifdef  UNIX
            CALL UVCOPY(NWS, ICNTL(IPBNAM+1), BNKNAM(IADB))
#endif
#ifdef  UNIX
            CALL UCEBC(4*NWS, BNKNAM(IADB))
#endif
#ifdef  UNIX
          ENDIF
#endif
                  IPBNAM      = IPBNAM + NWS
 
                    NWBNK(1,IADB) = ICNTL(IOFNWB+1)
                    NWBNK(2,IADB) = ICNTL(IOFNWB+2)
                  IF(HCNTL(IOFKEY+1) .LT. 0 ) THEN
                    KEYBNK(1,IADB) = -1
                    KEYBNK(2,IADB) = 0
                  ELSE
                    KEYBNK(1,IADB) = HCNTL(IOFKEY+1) + IEOFT
                    NELMNT         = HCNTL(IOFKEY+2)
                    KEYBNK(2,IADB) = NELMNT
                  ENDIF
                    IOFNWB           = IOFNWB + 2
                    IOFKEY           = IOFKEY + 2
C
C                                                              /// DEBUG
C                                            WRITE(6,9005) I,IOFKEY,IADB
C     .                                           ,(KEYBNK(K,IADB),K=1,4)
C 9005                                  FORMAT(' %TBREAD..I(NB),IOFKEY,',
C     .                                     'IADB,KEYBNK(1-4)=',/10X,7I6)
  400           CONTINUE
              ENDIF
C
C  ----- Copy to the Link_Table: LINK, Element_Key_Table : NWELM ---
C  -----                       and     Address_Table: IADTBL  -------
C
              IF( NOELMT .GT. 0 ) THEN
                HDOFT   = NWSUB(1,IPSUB)
C                                                            //// DEBUG
                                   IDOFT = 2*HDOFT
C                                   WRITE(6,9100) NOELMT, IDOFT, IOFADT
C9100                    FORMAT(' %TBREAD..NOELMT,IDOFT,IOFADT = ',3I8)
C
                DO 500 I = 1, NOELMT
                  IADD  = I + IEOFT
                  IOFADT         = IOFADT + 1
                  IADTBL(1,IADD) = ICNTL(IOFADT)
                  IADTBL(2,IADD) = 0
                  IADTBL(3,IADD) = 0
                  IOFELM         = IOFELM + 1
                  NWELM(1,IADD)  = ICNTL(IOFELM)
                  IOFELM         = IOFELM + 1
                  NWELM(2,IADD)  = ICNTL(IOFELM)
C
                  IOFLNK       = IOFLNK + 1
                  LINK(1,IADD) = HCNTL(IOFLNK)
                  IOFLNK       = IOFLNK + 1
                  IF( HCNTL(IOFLNK) .LT. 0 ) THEN
                      LINK(2,IADD) = HCNTL(IOFLNK)
                  ELSE
                      LINK(2,IADD) = HCNTL(IOFLNK) + IEOFT
                  ENDIF
C                                                            //// DEBUG
C                   WRITE(6,9110) I,IADD,LINK(2,IADD),IADTBL(1,IADD)
C9110               FORMAT(' %TBREAD..I,IADD,LINK,IADTBL(1,IADD)=',4I8)
C
 
  500           CONTINUE
              ENDIF
C
C  ---------------------------------------------------------------
C           Read Data_Block into the Buffer IBUFF
C  ---------------------------------------------------------------
C
              IDSTA   = (HDOFT + 1)/2 +1
C                                   ======  ============
#ifdef  UNIX
C	print *,' NRWFMT(1)=',NRWFMT(1)
        IF( MOD(NRWFMT(1),20).EQ.0 .OR. MOD(NRWFMT(1),20).EQ.4 
     >      .OR. NRWFMT(1).EQ.5 ) THEN
                TBREDF = TBxRED( LUN, NWORDS, IBUFF(IDSTA))
        ELSEIF( MOD(NRWFMT(1),20).EQ.1 ) THEN
          TBREDF = TBxRED( NWSUB(2,IPSUB), NWORDS, IBUFF(IDSTA))
        ELSE
          IKEEP0 = IBUFF(IDSTA-1)
          TBREDF = TBxRED( NWSUB(2,IPSUB),IBUFF(IDSTA-1),
     .             IBUFF(IDSTA))
          NWORDS=IBUFF(IDSTA-1)
          IBUFF(IDSTA-1) = IKEEP0
C        PRINT *,' NWORDS =',NWORDS,' IBUFF=',(IBUFF(K),K=IDSTA,IDSTA+4)
        ENDIF
#endif
              IF( .NOT.TBREDF ) THEN
                IRET  = -1000
                RETURN
              ENDIF
C
C  ----- If TBxRED = .TRUE., then  set IRET = Sub_Buffer ID. --------
C
C                                                           //// DEBUG
C                                                  WRITE(6,9120) IDSTA
C9120                                   FORMAT(' %TBREAD..IDSTA = ',I6)
C                                       CALL DMPARY(NWORDS,IBUFF(IDSTA),
C     .                                      ' %TBREAD..row data dump')
 
              IRET  = IDRET
C
C  -------------- Initialize the DSB_Parameters -----------------
C
              HWORDS         = NWORDS*2
              NWSUB(3,IPSUB) = HWORDS
              IDSUB(4,IPSUB) = NOBANK
              IDSUB(7,IPSUB) = NOELMT
              IDSUB(8,IPSUB) = 0
 
              NOREAD(IPSUB)  = NOREAD(IPSUB) + 1
              IDSIZE(IPSUB)  = HWORDS
 
C  ================================================================
C       IF the DSB_Name does not exist, then skip one record.
C  ================================================================
C       ******
         ELSE
C       ******
C  --------- Check the free space whether there is enough ----------
C  --------- for the input data. If not, then Fatal error Stop]]]---
 
            IF( ISKIP .EQ. 2 ) THEN
              IF( NWFREE .LT. NWRDS ) THEN
 
                NWFREE= NWFREE/2
                INEED = NWRDS - NWFREE
                WRITE(MESAGE(1),9031)
 9031           FORMAT('  Data Buffer is too small')
                WRITE(MESAGE(2),9032) INEED
                WRITE(MESAGE(3),9033) NWRDS
                WRITE(MESAGE(4),9034) NWFREE
 9032           FORMAT(I8,' words(I*4) more space is required to read')
 9033           FORMAT(I8,' words(I*4) data to the working area')
 9034           FORMAT(' Present free space is',I8,' words(I*4).')
 
                CALL TBxERR( 600,'TBREAD', 4)
              ENDIF
 
              IDSTA   = (NWSUB(1,NOSUB) + NWSUB(2,NOSUB))/2 + 1
C                                   ======  ============
#ifdef  UNIX
        IF( MOD(NRWFMT(1),20).EQ.0 .OR. MOD(NRWFMT(1),20).EQ.4 ) THEN
                 TBREDF = TBxRED(LUN, NWORDS, IBUFF(IDSTA))
        ELSE
           TBREDF = TBxRED(NWSUB(2,NOSUB), NWORDS, IBUFF(IDSTA))
        ENDIF
#endif
              IF( .NOT.TBREDF ) THEN
                IRET  = -1000
                RETURN
              ENDIF
 
C  ----- If the record is a passed one, then copy them onto LUN. ----
 
              WRITE(LUOUT) NWCNTL,(ICNTL(I),I=1,NWCNTL)
              WRITE(LUOUT) NWORDS,(IBUFF(I),I=IDSTA,IDSTA+NWORDS-1)
 
              NOPASS   = NOPASS + 1
 
              WRITE(MESAGE(1),9041)
 9041         FORMAT(' The following record was passed')
              WRITE(MESAGE(2),9042) RECNAM
 9042         FORMAT(' Record name = ',A64)
              CALL TBxERR( 100,'TBREAD', 2)
            ELSE
              IF( .NOT.TBxSKP( LUN ) ) THEN
                  IRET  = -1000
                  RETURN
              ENDIF
 
              NOSKIP   = NOSKIP + 1
 
              WRITE(MESAGE(1),9051)
 9051         FORMAT(' The following record was skipped')
              WRITE(MESAGE(2),9042) RECNAM
              CALL TBxERR( 100,'TBREAD', 2)
            ENDIF
 
            GO TO 1000
 
C       *******
         ENDIF
C       *******
 
#ifdef  UNIX
         IF( (MOD(NRWFMT(1),20).NE.0 .OR. MOD(NRWFMT(1),20).EQ.4) 
     >  	.AND. IRET.GT.0 ) THEN
#endif
#ifdef  UNIX
           CALL TBxUNX(IRET)
#endif
#ifdef  UNIX
           CALL TBxSRT(IPSUB, IR1)
#endif
#ifdef  UNIX
         ENDIF
#endif
 
         RETURN
         END
