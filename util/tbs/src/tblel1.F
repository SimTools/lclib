C *********************************************************************C
C*                                                                     *
C*============================================                         *
C* Subroutine TBLEL1(ID,BNAME,NE,MAXWRD,FORM)                          *
C*===========================================                          *
C*(Purpose)                                                            *
C*   Dump contents of TBS_Buffer.                                      *
C*(Input)                                                              *
C*   ID     : DSB ID.                                                  *
C*   BNAME  : Bank_name.                                               *
C*   NE     : Element no.                                              *
C*   MAXWRD : # of words to be listed, list all data when = 0.         *
C*   FORM   : Character*4 of output format.                            *
C*            FORM = 'I*4 ', 'I*2 ', 'R*4 ','A*4',or 'HEXA'            *
C*                                                                     *
C*(Used COMMON)                                                        *
C*  /TBSBNK/, /TBSBUF/, /TBSPRM/                                       *
C*(Author)                                                             *
C*       A.Miyamoto     12-Feb-1985                                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLEL1(ID,BNAME,NE,MAXWRD,FORM)
 
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsprm.inc"
#include "tbsmes.inc"
C
      CHARACTER*(*) BNAME
      CHARACTER     STRING*140,TNAME*64
      CHARACTER*4   CHARA(5)
      CHARACTER*3   FORM, TFORM
C     INTEGER*4     ITEMP(5)
 
C*
C*(1) Check bank_name length.
C*
      LBNAM   = LEN(BNAME)
      IF(LBNAM.GT.64) THEN
 
           WRITE(MESAGE(1),9001) LBNAM
 9001      FORMAT(' The bank name of ',I3,' characters is too long.')
           WRITE(MESAGE(2),9002)
 9002      FORMAT(' The bank name is truncated by 64 characters.')
           LBNAM = 64
           WRITE(MESAGE(3),9003) BNAME(1:LBNAM)
 9003      FORMAT(' Bank name = "',A,'"')
           CALL TBxERR( 200,'TBLEL1', 3)
 
      END IF
C
C*   1.1  Convert to the upper case character.
 
         TNAME = BNAME
 
         CALL UCUPER( LBNAM, TNAME)
 
         TFORM = FORM
         CALL UCUPER( 3, TFORM)
C
C    1.2 Check the input parameter
C
      IF(TFORM.NE.'I*4'.AND.TFORM.NE.'I*2'.AND.TFORM.NE.'R*4'.AND.
     .   TFORM.NE.'A*4'.AND.TFORM.NE.'HEX') THEN
 
           WRITE(MESAGE(1),9015) FORM
 9015      FORMAT(' Invalid FORM selected, FORM = ',A4)
           WRITE(MESAGE(2),9016) FORM
 9016      FORMAT(' Assuming FORM = I*2 ')
           CALL TBxERR( 200,'TBELE1', 2)
 
           TFORM  = 'I*2'
      ENDIF
 
C*
C*(2) Get IPSUB.. Pointer to DSB.
C*
C     -----------------========
      CALL TBxFRD ( ID, IPSUB )
C     -----------------========
      IF ( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9011) ID
 9011      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBELE1', 1)
 
        RETURN
      ENDIF
C*
C*(3) Get IPBNK, and ISRT
C*
C     ---------------------------====================
      CALL TBxFBK ( IPSUB, TNAME, ISRT, IPBNK, IRET )
C     ---------------------------====================
 
      IF ( IRET  .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9021) ID
 9021        FORMAT(' The following bank does not exist in the REC-ID ('
     .              ,I3,' )')
             WRITE(MESAGE(2),9022) TNAME
 9022        FORMAT(' Bank name = ',A64)
             CALL TBxERR( 200,'TBELE1', 2)
 
        RETURN
      ENDIF
C*
C*(4) Get IPELM, pointer to element.
C*
C     ------------------------=============
      CALL TBxFEL ( IPBNK, NE, IPELM, IRET )
C     ------------------------=============
      IF ( IRET  .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9031) NE,ID
 9031        FORMAT(' Element#',I4,' does not exist in the REC-ID ('
     .              ,I3,' )')
             WRITE(MESAGE(2),9022) TNAME
             CALL TBxERR( 200,'TBELE1', 2)
 
        RETURN
      ENDIF
C*
C*(5) Write dump title.
C*
      WRITE(LUNIT,300) TFORM,SUBNAM(IPSUB), ID, BNKNAM(IPBNK), NE
300   FORMAT(//,' *** TBS dump bank element data ***',
     >          ' in ',A3,' format ',/,
     >       5X,' DSB_name  = ',A64,' DSB ID    =',I4,/,
     >       5X,' Bank_name = ',A64,' Element # =',I4)
C*
C*(6) Get header part of element data,  and write them.
C*
      IPDATA = (IADTBL(1,IPELM)+NWSUB(1,IPSUB)+1)/2
      ISPACE = (NWELM(1,IPELM)+1)/2
      NWORDS = (NWELM(2,IPELM)+1)/2
 
      WRITE(LUNIT,400) ISPACE, NWORDS
400   FORMAT(5X,' Temp. space =',I5,' #WORDS=',I8)
      WRITE(LUNIT,430)
      WRITE(LUNIT,420)
      WRITE(LUNIT,430)
420   FORMAT('  WRD#    Addr|        E L E M E N T      D A T A  ')
430   FORMAT(' ----|--------|-------------|-------------|',
     >       '-------------|-------------|-------------|')
C*
C*(7) Dump data.
C*
      IADD   = IPDATA -1
      IF( MAXWRD .LE. 0 )  MAXWRD = NWORDS/2 + 1
      MINWRD = NWORDS - MAXWRD
      IADDMX = IADD + NWORDS
C
 
      DO 500 IWRD = 1, MAXWRD, 5
        IADD   = IPDATA + IWRD - 1
        IADD2  = 2*IADD
 
        I1ST   = IADD
        ILST   = IADD + 4
        IF(ILST.GT.IADDMX) ILST = IADDMX
 
        IF(TFORM.EQ.'I*2') THEN
          I1ST = 2*I1ST - 1
          ILST = 2*ILST
          WRITE(LUNIT,510) IWRD,IADD,(HBUFF(J),J=I1ST, ILST)
510       FORMAT(1X,I5,I9,10(2X,I7,:))
 
        ELSEIF(TFORM.EQ.'I*4') THEN
          WRITE(LUNIT,520) IWRD,IADD,(IBUFF(J),J=I1ST,ILST)
520       FORMAT(1X,I5,I9,5(1X,I13,:))
 
        ELSEIF(TFORM.EQ.'R*4') THEN
          WRITE(LUNIT,530) IWRD,IADD,(RBUFF(J),J=I1ST,ILST)
530       FORMAT(1X,I5,I7,5(1X,G13.6,:))
 
        ELSEIF(TFORM(1:3).EQ.'A*4'.OR.TFORM(1:3).EQ.'HEX') THEN
          DO 540 I = I1ST,ILST
            K = I - I1ST + 1
            CALL TBxITC( IBUFF(I), CHARA(K))
540       CONTINUE
          STRING = '   '
 
          WRITE(STRING,550) IWRD,IADD,(IBUFF(J),J = I1ST,ILST)
550       FORMAT(1X,I5,I7,5(6X,Z8,:))
          WRITE(LUNIT,560) STRING(1:83),(CHARA(J),J=1,K)
560       FORMAT(A83,5(2X,A4,:))
        ENDIF
500   CONTINUE
 
 
      DO 600 IWRD = MINWRD, NWORDS, 5
        IF( IWRD .EQ. MINWRD ) WRITE(LUNIT,565)
565     FORMAT(10X,'..........')
        IADD   = IPDATA + IWRD - 1
        IADD2  = 2*IADD
 
        I1ST   = IADD
        ILST   = IADD + 4
        IF(ILST.GT.IADDMX) ILST = IADDMX
 
        IF(TFORM.EQ.'I*2') THEN
          I1ST = 2*I1ST - 1
          ILST = 2*ILST
          WRITE(LUNIT,510) IWRD,IADD,(HBUFF(J),J=I1ST, ILST)
 
        ELSEIF(TFORM.EQ.'I*4') THEN
          WRITE(LUNIT,520) IWRD,IADD,(IBUFF(J),J=I1ST,ILST)
 
        ELSEIF(TFORM.EQ.'R*4') THEN
          WRITE(LUNIT,530) IWRD,IADD,(RBUFF(J),J=I1ST,ILST)
 
        ELSEIF(TFORM(1:3).EQ.'A*4'.OR.TFORM(1:3).EQ.'HEX') THEN
          DO 640 I = I1ST,ILST
            K = I - I1ST + 1
            CALL TBxITC( IBUFF(I), CHARA(K))
640       CONTINUE
          STRING = '   '
 
          WRITE(STRING,550) IWRD,IADD,(IBUFF(J),J = I1ST,ILST)
          WRITE(LUNIT,560) STRING(1:83),(CHARA(J),J=1,K)
        ENDIF
600   CONTINUE
 
      RETURN
      END
