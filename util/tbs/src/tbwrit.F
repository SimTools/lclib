C *********************************************************************C
C*                                                                     *
C*===================================                                  *
C* Subroutine TBWRIT( LUN, ID, IRET )                                  *
C*===================================                                  *
C*(Purpose)                                                            *
C*   Write Data of DSB: ID on the unit LUN.                            *
C*(Input)                                                              *
C*   LUN      : Logical_Unit_Number for the Output Device              *
C*   ID       : ID_Number of the DSB                                   *
C*(Output)                                                             *
C*   IRET     : Return_Flag                                            *
C*        > 0 : Number of I*4 words written                            *
C*              (= Data block + Control block )                        *
C*(Used COMMON)                                                        *
#include "ctlexp.inc"
#include "bnkexp.inc"
#include "bufexp.inc"
C*(Author)                                                             *
C*       S.Kawabata     June 2 '84                                     *
C*       S.Kawabata     July 16 '85     Version up  V3.0               *
C*       S.Kawabata     Nov. 27 '85     Version up  V4.0               *
C*       A. Miyamoto    Mar. 18 '87     Use /TBSIOB/ (V4.03)           *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBWRIT( LUN, ID, IRET )
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsctl.inc"
#include "tbsmes.inc"
#include "sswork.inc"
#include "tbssum.inc"
#include "tbsiob.inc"
 
      CHARACTER*64 TNAME
      DATA NPRINT / 0 /, MXPRNT / 10 /
C
C  -------------------------------------------------------------------
C                 Test whether Record ID exists in any DSB.
C  -------------------------------------------------------------------
CTIME                               CALL CLOCK( TIM0, 0, 1 )
C                          =====
         CALL TBxFRD( ID,  IPSUB )
 
CTIME                               CALL CLOCK( TIM1, 0, 1 )
CTIME                               TIME  = TIM1 - TIM0
CTIME                               WRITE(6,8000) TIME
CTIME 8000                   FORMAT(1X,F10.3,' sec to find Record ID.')
 
CTIME                               CALL CLOCK( TIM0, 0, 1 )
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBWRIT', 1)
 
           IRET     = -1
           RETURN
         ENDIF
 
         IASH   = IDSUB(11,IPSUB)
         IF( IASH .GT. 0 ) THEN
             ISSH  = IDSUB(10,IPSUB)
             TNAME = SHRNAM( IASH, ISSH)
         ELSE
             TNAME = SUBNAM(IPSUB)
         ENDIF
 
CTIME                               CALL CLOCK( TIM1, 0, 1 )
CTIME                               TIME  = TIM1 - TIM0
CTIME                               WRITE(6,8010) TIME
CTIME 8010               FORMAT(1X,F10.3,' sec to find Record name.')
 
CTIME                               CALL CLOCK( TIM0, 0, 1 )
C
C  -------------------------------------------------------------------
C     Get General information of the Control_Block from the COMMON
C    /TBSBNK/.
C  -------------------------------------------------------------------
C  ---------------- Get General Information --------------------------
C
          ICNTL( 3) = NWSUB(2,IPSUB)
          NWS       = 16
C                                  =======
          CALL TBxINT( TNAME, NWS, ICNTL(4))
C
          IBOFT     = IDSUB(2,IPSUB)
          MXBANK    = IDSUB(3,IPSUB)
          NOBANK    = IDSUB(4,IPSUB)
          IEOFT     = IDSUB(5,IPSUB)
          MXELMT    = IDSUB(6,IPSUB)
          NOELMT    = IDSUB(7,IPSUB)
          HCNTL(39) = NOBANK
          HCNTL(40) = MXBANK
          HCNTL(41) = NOELMT
          HCNTL(42) = MXELMT
C
C  -------- Get Offsets of Sort,Bank_Name,Bank_Key,Link ------------
C  --------    Element_Key_Table and Address_Table      ------------
C  -------- and pack Bank_names                         ------------
 
          IOFSRT    = 50
          IOFNAM    = (IOFSRT + NOBANK + 1)/2
          IOFBKN    = IOFNAM
          IF( NOBANK .GT. 0 ) THEN
             DO  100     NB  = 1, NOBANK
               IPBNK         = NB + IBOFT
               CALL TBxLEN( BNKNAM(IPBNK), NWS )
               IOFBKN        = IOFBKN + 1
               ICNTL(IOFBKN) = NWS
               IPNT          = IOFBKN + 1
C                                              ============
               CALL TBxINT( BNKNAM(IPBNK), NWS, ICNTL(IPNT))
               IOFBKN        = IOFBKN + NWS
  100        CONTINUE
          ENDIF
          IOFNWB    = IOFBKN
          IOFKEY    = 2*(IOFBKN + NOBANK*2)
          IOFLNK    = IOFKEY + 2*NOBANK
          IOFELM    = (IOFLNK + 2*NOELMT)/2
          LINKAD    = 1
          IOFADT    = IOFELM + 2*NOELMT
C
          HCNTL(43) = IOFSRT
          HCNTL(44) = IOFNAM
          HCNTL(45) = IOFNWB
          HCNTL(46) = IOFKEY
          HCNTL(47) = IOFLNK
          HCNTL(48) = IOFELM
          HCNTL(49) = IOFADT
C
C  -------------------------------------------------------------------
C     Make the Control_Block and the output ordering table NBEGIN(i)
C    and NFINAL(i).
C    Do the following steps for NOBANK times.
C  -------------------------------------------------------------------
C
         NWORDS    = 0
         NREC     = 0
C
C  --- Copy Sort_Table to the Control_Block at a time. (only once) --
C
         IF( NOBANK .GT. 0 ) THEN
           DO  500     NB  = 1, NOBANK
             IOFSRT        = IOFSRT + 1
             HCNTL(IOFSRT) = ISORT( IBOFT+NB ) - IBOFT
  500      CONTINUE
 
C             ******
           DO  1000    NB  = 1, NOBANK
C             ******
 
                IPBNK         = NB + IBOFT
 
C  --- Copy Bank_Key to the Control_Block  --------------------------
 
                IOFNWB        = IOFNWB + 1
                ICNTL(IOFNWB) = NWBNK(1,IPBNK)
                IOFNWB        = IOFNWB + 1
                ICNTL(IOFNWB) = NWBNK(2,IPBNK)
C
                IPNT          = KEYBNK(1,IPBNK)
C
                IOFKEY        = IOFKEY + 2
                NELMNT        = KEYBNK(2,IPBNK)
                HCNTL(IOFKEY) = NELMNT
C                                                             /// DEBUG
C                                 WRITE(6,9000) NB,NELMNT,BNKNAM(IPBNK)
C9000                     FORMAT(1X,'==== Loop for No. of Banks =====',
C    -                      /1X,'TBWRIT NB,NELMNT,BNKNAM =',2I6,2X,A32)
C
                IF( NELMNT .LE. 0 ) THEN
                    HCNTL(IOFKEY-1) = -1
C
                ELSE
                    HCNTL(IOFKEY-1) = LINKAD
C
C   ------- Copy Link_Table, Address_Table and Ordering Data.  -------
C   -------    Do the following steps for NELMNT times.        -------
C
                    DO    900   NE  = 1, NELMNT
C
C                                  Copy Link_Table to the Control_Block
C
                       IOFLNK        = IOFLNK + 1
                       LINKAD        = LINKAD + 1
                       HCNTL(IOFLNK) = LINK(1,IPNT)
                       NEXT          = LINK(2,IPNT)
C
                       IOFLNK        = IOFLNK + 1
                     IF( NE .EQ. NELMNT ) THEN
                       HCNTL(IOFLNK)= -1
                     ELSE
                       HCNTL(IOFLNK)= LINKAD
                     ENDIF
C
C                        Get Address and Parameters of the Bank_element.
C
                       HPDATA       = IADTBL(1,IPNT) + NWSUB(1,IPSUB)
                       HWDSB        = NWELM(2,IPNT)
                       HPEND        = HPDATA + HWDSB - 1
C
                       NREC         = NREC + 1
                       NBEGIN(NREC) = (HPDATA+1)/2
                       NFINAL(NREC) = (HPEND+1)/2
		
C
C                      Change Address of the Bank_Element and store it.
C
                       IOFELM       = IOFELM + 1
                       ICNTL(IOFELM)= 2*( (HWDSB+1)/2 )
                       IOFELM       = IOFELM + 1
                       ICNTL(IOFELM)= HWDSB
                       IOFADT       = IOFADT + 1
                       ICNTL(IOFADT)= 2*NWORDS + 1
C
C*    4. Preparation for the next Bank_Element.
C
                       IPNT         = NEXT
                       NWORDS       = NWORDS + (HWDSB+1)/2
C
  900             CONTINUE
                ENDIF
C*****
 1000      CONTINUE
C*****
         ENDIF
C
C  ------------------------------------------------------------------
C            Write the Control_Block on the Unit LUN.
C  ------------------------------------------------------------------
C
         ICNTL(1)         = NWORDS*2
CC       ICNTL(2)         = 403    D940517  Prepared for UNIX version.
         ICNTL(2)         = 501
         NWCNTL           = IOFADT
 
CTIME                               CALL CLOCK( TIM1, 0, 1 )
CTIME                               TIME  = TIM1 - TIM0
CTIME                               WRITE(6,8100) TIME
CTIME 8100               FORMAT(1X,F10.3,' sec to pack CNTL Block.')
 
CTIME                               CALL CLOCK( TIM0, 0, 1 )
C                                                            /// DEBUG
C                                                    WRITE(6,9200) LUN
C9200                  FORMAT(1X,'TBWRIT Write Control_Block  LUN=',I5)
 
         NTWRIT        = NTWRIT + 1
         NOWRIT(IPSUB) = NOWRIT(IPSUB) + 1
#ifdef UNIX
         Call TB_Set_Buffer ( LUN, MXWORK, IWORK )
         Call TB_Write_Buffer ( 1, NWCNTL )
         Call TB_Write_Buffer ( NWCNTL, ICNTL )
#else
         WRITE(LUN, IOSTAT=IOS, ERR=5000)
     .                NWCNTL,(ICNTL(K),K=1,NWCNTL)
 
CTIME                               CALL CLOCK( TIM1, 0, 1 )
CTIME                               TIME  = TIM1 - TIM0
CTIME                               WRITE(6,8110) TIME
CTIME 8110               FORMAT(1X,F10.3,' sec to write CNTL Block')
 
CTIME                               CALL CLOCK( TIM0, 0, 1 )
#endif
C
C  -----------------------------------------------------------------
C            Write the Data_Sub_Buffer on the Unit LUN.
C  -----------------------------------------------------------------
C
C                                                              /// DEBUG
C                                                      WRITE(6,9300) LUN
C9300                      FORMAT(1X,'TBWRIT Write Data_Block  LUN=',I5)
C
#ifdef UNIX
         Call TB_Write_Buffer ( 1, NWORDS )
         Do L = 1, Nrec
           NBGN = NBEGIN ( L )
           NWS = NFINAL ( L ) - NBGN + 1
           Call TB_Write_Buffer ( NWS, IBUFF(NBGN) )
         End Do
         Call TB_Flush_Buffer ( Ntot )
#else
         IF( NWORDS .LE. MXWORK ) THEN
             IBGN     = 1
             DO 600 L = 1, NREC
                NBGN  = NBEGIN(L)
                NWS   = NFINAL(L) - NBGN + 1
                CALL UVCOPY( NWS, IBUFF( NBGN ), IWORK( IBGN ))
                IBGN  = IBGN + NWS
  600        CONTINUE
 
CTIME                               CALL CLOCK( TIM1, 0, 1 )
CTIME                               TIME  = TIM1 - TIM0
CTIME                               WRITE(6,8111) TIME
CTIME 8111               FORMAT(1X,F10.3,' sec to copy  Data Block')
CTIME                               CALL CLOCK( TIM0, 0, 1 )
 
             WRITE(LUN, IOSTAT=IOS, ERR=5000)
     .                  NWORDS,(IWORK(K),K=1,NWORDS)
         ELSE
 
             WRITE(LUN, IOSTAT=IOS, ERR=5000)
     .              NWORDS,((IBUFF(K),K=NBEGIN(L),NFINAL(L)),L=1,NREC)
 
             NPRINT = NPRINT + 1
             IF( NPRINT .LE. MXPRNT ) THEN
                 WRITE(MESAGE(1),9501)
 9501            FORMAT(' Size of /SSWORK/ is too small')
                 WRITE(MESAGE(2),9502) NWORDS
 9502            FORMAT(' Required size is ',I10)
                 CALL TBxERR( 200,'TBWRIT', 2)
             ENDIF
         ENDIF
#endif 
CTIME                               CALL CLOCK( TIM1, 0, 1 )
CTIME                               TIME  = TIM1 - TIM0
CTIME                               WRITE(6,8120) TIME
CTIME 8120               FORMAT(1X,F10.3,' sec to write Data Block')
 
 
         IRET    = NWCNTL + NWORDS
         RETURN
 
 5000    IF( IOS .GT. 0 ) THEN
             IRET = -2
         ELSE
             IRET = -1000
         ENDIF
         RETURN
         END


