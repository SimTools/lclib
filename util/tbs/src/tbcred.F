C *********************************************************************C
C*                                                                     *
C*===============================                                      *
C* Subroutine TBCRED( IDREC, IVERS, IRET, NDATIM )                     *
C*===============================                                      *
C*(Purpose)                                                            *
C*   Read Data from the data base.                                     *
C*(Input)                                                              *
C*   IDREC    : Record ID to read.                                     *
C*   IVERS    : Version # to get.                                      *
C*         = 0 to get latest version data.                             *
C*         > 0 to get specified version data.                          *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*     IRET >     0 : Version # of obtained data.                      *
C*          =    -1 : Record ID not allocated.                         *
C*          =    -2 : Specified version data not exists.               *
C*          =    -3 : Requested record not exists on the data base.    *
C*   NDATIM(2) : Date and Time of record.
C*(Used COMMON)                                                        *
#include "ctlexp.inc"
#include "bnkexp.inc"
#include "bufexp.inc"
C*(Author)                                                             *
C*    A. Miyamoto  18 Mar. '87  Copied from TBREAD                     *
C*                                                                     *
C***********************************************************************
 
#ifdef  MSP
      SUBROUTINE TBCRED( IDREC, IVERS, IRET, /NDATIM/ )
#endif
#ifdef  UNIX
      SUBROUTINE TBCRED( IDREC, IVERS, IRET )
#endif
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsctl.inc"
#include "tbspas.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
#include "tbcdat.inc"
         CHARACTER*64 RECNAM
         INTEGER*4    NDATIM(2)
 
 
#ifdef  UNIX
         NUMARG=3
#endif
#ifdef  UNIX
         GO TO 1000
#endif
#ifdef  UNIX
         ENTRY TBCREDx(IDREC, IVERS, IRET, NDATIM)
#endif
#ifdef  UNIX
         NUMARG=4
#endif
#ifdef  MSP
         NUMARG = NARG(0)
#endif
C  =================================================================
C     Read Control_Block Data into the Buffer ICNTL
C  =================================================================
C ****                         ======  =====
C
 1000    CONTINUE
         CALL TBCxRD( IDREC, IVERS, 'C', NWCNTL, ICNTL, IRET)
         IF(IRET.LT.0)  RETURN
         IKVRS  = IRET
C ****
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
         CALL TBxCHA( ICNTL(4), 16,  RECNAM )
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
                  CALL TBxCHA( ICNTL(IPBNAM+1), NWS,  BNKNAM(IADB))
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
C    .                                           ,(KEYBNK(K,IADB),K=1,4)
C9005                                  FORMAT(' %TBREAD..I(NB),IOFKEY,',
C    .                                     'IADB,KEYBNK(1-4)=',/10X,7I6)
  400           CONTINUE
              ENDIF
C
C  ----- Copy to the Link_Table: LINK, Element_Key_Table : NWELM ---
C  -----                       and     Address_Table: IADTBL  -------
C
              IF( NOELMT .GT. 0 ) THEN
                HDOFT   = NWSUB(1,IPSUB)
C                                                            //// DEBUG
C                                   IDOFT = 2*HDOFT
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
              CALL TBCxRD(IDREC,IVERS, 'D', NWORDS, IBUFF(IDSTA), IRET)
 
C              IF( .NOT.TBxRED( LUN, NWORDS, IBUFF(IDSTA)) ) THEN
C                IRET  = -1000
C                RETURN
C              ENDIF
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
 
 
C       *******
         ENDIF
C       *******
         IF( NUMARG .GE. 4 ) THEN
           NDATIM(1) = NCDATE
           NDATIM(2) = NCTIME
         ENDIF
C
         IRET = IKVRS
 
         RETURN
C
         END
