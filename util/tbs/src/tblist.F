C **********************************************************************
C*                                                                     *
C*=========================                                            *
C* Subroutine TBLIST( ID )                                             *
C*=========================                                            *
C*(Purpose)                                                            *
C*   To list the contents of the record ID.                            *
C*(Input)                                                              *
C*   ID       : ID_Number of the record                                *
C*(Used COMMON)                                                        *
C*     /TBSBNK/ and /TBSBUF/  Further information, see TBREAD.         *
C*(Author)                                                             *
C*       S.Kawabata     May 23 '84                                     *
C*       S.Kawabata     Nov.27 '85     Version V4.0                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLIST( ID )
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsmes.inc"
#include "tbsprm.inc"
      CHARACTER*64  DSB, REC, BANK
      CHARACTER*80  FORM
      INTEGER*2     LISTE(1000)
 
C  --------------------------------------------------------------------
C                    Check the several Parameters
C  --------------------------------------------------------------------
C                         =====
      CALL TBxFRD( ID, IPSUB )
C
      IF( IPSUB .LT. 0 ) THEN
 
          WRITE(MESAGE(1),9001) ID
 9001     FORMAT(' Record ID(=',I5,' ) does not exist.')
          CALL TBxERR( 200,'TBGET', 1)
 
          RETURN
      ENDIF
 
      DSB   = SUBNAM( IPSUB )
      IDDSB = IDSUB(1,IPSUB )
      I     = IDSUB(11,IPSUB)
      IF( I .LT. 0 ) THEN
          WRITE(MESAGE(1),9002) IDDSB
 9002     FORMAT(' Nothing on the DSB ID =',I5)
          CALL TBxERR( 200,'TBGET', 1)
          RETURN
      ELSEIF( I .GT. 0 ) THEN
          J     = IDSUB(10,IPSUB)
          REC   = SHRNAM( I, J )
          IDREC = IDSHR( I, J )
      ELSE
          REC   = DSB
          IDREC = IDDSB
      ENDIF
 
      WRITE( LUNIT, 9000)
 9000 FORMAT(/1H1)
      WRITE( LUNIT, 9100)
 9100 FORMAT(/1H )
      WRITE( LUNIT, 9110) ID
 9110 FORMAT(1X,'********* List of contents of the record ID(',I5,
     .          ' ) *********',
     .      /10X,'To get this CALL TBLIST(ID).')
      WRITE( LUNIT, 9100)
      WRITE(LUNIT,9120) DSB,IDDSB,NWSUB(2,IPSUB)/2
 9120 FORMAT(1X,'(1) Data Sub Buffer',
     .          /5X,'DSB Name       = ',A64,
     .          /5X,'DSB ID         = ',I10,
     .          /5X,'Allocated size = ',I10)
 
      NOBNK   = IDSUB(4,IPSUB)
      WRITE( LUNIT, 9100)
      WRITE(LUNIT,9130) REC,IDREC,NWSUB(3,IPSUB)/2,NOBNK,
     .                            IDSUB(7,IPSUB)
 9130 FORMAT(1X,'(2) Current Record on the DSB',
     .          /5X,'Record Name    = ',A64,
     .          /5X,'Record ID      = ',I10,
     .           5X,'Data size      = ',I10,
     .          /5X,'No of banks    = ',I10,
     .           5X,'No of elements = ',I10)
 
      IF( NOBNK .LE. 0 ) RETURN
 
      WRITE( LUNIT, 9100)
      WRITE(LUNIT,9200)
 9200 FORMAT(1X,'(3) Bank and element list')
      WRITE(LUNIT,9210)
 9210 FORMAT(1X,
     .      '*------',6('----------'),
     .      '+------------+------------+------------*')
      WRITE(LUNIT,9220)
 9220 FORMAT(1X,
     .      '!',29X,'Bank Name',28X,
     .      '! # of elmnt ! Alloc size !  Real size !')
             WRITE(LUNIT,9230)
             WRITE(LUNIT,9225)
 9225        FORMAT(1X,'!',41X,'List of element numbers',41X,'!')
      WRITE(LUNIT,9210)
      LINE  = 20
 
      IOFBNK    = IDSUB(2,IPSUB)
 
      DO 1000 I = 1, NOBNK
 
         IAD    = ISORT(I + IOFBNK)
         BANK   = BNKNAM(IAD)
         NOELM  = 0
         IF( KEYBNK(2,IAD) .GT. 0 ) THEN
             IP           = KEYBNK(1,IAD)
             MXNEL        = KEYBNK(2,IAD)
             NOELM        = 1
             NWALC        = NWELM(1,IP)
             NWBK         = NWELM(2,IP)
             LISTE(NOELM) = LINK(1,IP)
             DO 500 NE = 1,MXNEL
                IF( LINK(2,IP) .LE. 0 ) GO TO 550
                IP           = LINK(2,IP)
                NOELM        = NOELM + 1
                NWALC        = NWALC + NWELM(1,IP)
                NWBK         = NWBK  + NWELM(2,IP)
                LISTE(NOELM) = LINK(1,IP)
  500        CONTINUE
         ENDIF
 
  550    LADD         = NOELM/15 + 3
         IF( MOD(NOELM,15) .NE. 0 ) LADD = LADD + 1
         LINE         = LINE + LADD
         IF( LINE .GE. 60 ) THEN
             WRITE(LUNIT,9000)
             WRITE(LUNIT,9210)
             WRITE(LUNIT,9220)
             WRITE(LUNIT,9230)
             WRITE(LUNIT,9225)
             WRITE(LUNIT,9210)
             LINE     = 5
         ENDIF
         WRITE(LUNIT,9250) BANK,NOELM,NWALC/2,NWBK/2
 9250    FORMAT(1X,'! ',A64,' !',3(I11,' !'))
 
         IF( NOELM .GT. 0 ) THEN
             WRITE(LUNIT,9230)
 9230        FORMAT(1X,
     .      '!-- - -',6(' - - - - -'),
     .      '+- - - - - - + - - - - - -+- - - - - --!')
             MSTAT    = 1
             DO 600 K = 0, NOELM-1
                MSTAT = 15*K + 1
                IF( MSTAT .GT. NOELM) GO TO 610
                MEND  = MSTAT + 14
                IF( MEND .GT. NOELM) MEND = NOELM
 
                NLST    = MEND - MSTAT + 1
                IREST   = 6*(15-NLST) + 3
                IF( K .EQ. 0 ) THEN
                    WRITE(FORM,9260) NLST,IREST
 9260               FORMAT('(1X,''! Element # ='',',
     .                          I2,'I6,',I2,'X,''!'')')
                ELSE
                    WRITE(FORM,9261) NLST,IREST
 9261               FORMAT('(1X,''!            '',',
     .                          I2,'I6,',I2,'X,''!'')')
                ENDIF
 
                WRITE(LUNIT,FORM) (LISTE(M),M = MSTAT,MEND)
 
  600        CONTINUE
  610        CONTINUE
 
         ENDIF
 
         WRITE(LUNIT,9210)
 
 1000 CONTINUE
 
      RETURN
      END
