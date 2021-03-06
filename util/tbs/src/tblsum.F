C *********************************************************************C
C*                                                                     *
C*==========================                                           *
C* Subroutine TBLSUM                                                   *
C*==========================                                           *
C*(Purpose)                                                            *
C*   To print the statistical information                              *
C*(Input)                                                              *
C*   none                                                              *
C*(Used COMMON)                                                        *
C*  /TBSBNK/ and /TBSSUM/                                              *
C*(Author)                                                             *
C*       S.KAWABATA     Sept. 27 '86                                   *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLSUM
 
#include "tbsbnk.inc"
#include "tbsprm.inc"
#include "tbssum.inc"
 
C     Dump MAXSUB, MAXBNK, MAXELM, MAXSIZ, NWFREE, and NOSUB
 
      NMAXS   = MAXSIZ/2
      NFREE   = NWFREE/2
 
      WRITE(LUNIT,9000)
 9000 FORMAT(/1H1,' ****** Tristan_Bank_System : Statistical',
     .   ' information ******',/5X,
     .   ' This information can be obtained by CALL TBLSUM.',
     .   //1H ,'(1) Sub_buffer parameters ')
      WRITE(LUNIT,9005) MAXSUB,MAXBNK,MAXELM,NMAXS,NFREE,NOSUB,NOSHR
 9005 FORMAT(5X,
     .    ' Maximum allocatable Sub_Buffer number(MAXSUB) =',I8,/,
     . 5X,' Maximum allocatable bank number      (MAXBNK) =',I8,/,
     . 5X,' Maximum allocatable element number   (MAXELM) =',I8,/,
     . 5X,' Allocated TBS buffer size            (MAXSIZ) =',I8,/,
     . 5X,' Unused TBS buffer size               (NWFREE) =',I8,/,
     . 5X,' Defined Sub_Buffer number            (NOSUB)  =',I8,/,
     . 5X,' Number of shared DSBs                (NOSHR)  =',I8)
C
C     Dump SUBNAM, NWSUB, and IDSUB
C
      IF(NOSUB.LE.0) RETURN
 
      WRITE(LUNIT,9100)
 9100 FORMAT(/1H )
      WRITE(LUNIT,9105)
 9105 FORMAT(1X,'(2) Data Sub Buffer List')
      WRITE(LUNIT,9110)
 9110 FORMAT(1X,
     .       '+--------+--------+------',6('----------'),'+-------+')
      WRITE(LUNIT,9115)
 9115 FORMAT(1X,'! DSB-ID ! SHR-ID !',23X,'DSB or Record   Name',23X,
     .          '!       !')
      WRITE(LUNIT,9110)
 
      DO 150 I = 1, NOSUB
         WRITE(LUNIT,9120) IDSUB(1,I),SUBNAM(I)
 9120       FORMAT(1X,'!',I7,' !',8X,'! ',A64,' !  DSB  !')
         IF( IDSUB(9,I) .GT. 0 ) THEN
            K     = IDSUB(10,I)
            DO 100 J = 1, IDSUB(9,I)
               WRITE(LUNIT,9125) IDSHR(J,K),SHRNAM(J,K)
 9125          FORMAT(1X,'!',8X,'!',I7,' ! ',A64,' ! Share !')
  100       CONTINUE
         ENDIF
         WRITE(LUNIT,9110)
  150 CONTINUE
 
 
      WRITE(LUNIT,9100)
      WRITE(LUNIT,9200)
 9200 FORMAT(1X,'(3) I/O Statistics')
      WRITE(LUNIT,9205) NTREAD,NREDER,NTWRIT,NOPASS,NOSKIP
 9205 FORMAT(5X,'Total number of reads         =', I10,
     .      /5X,'Total number of read error    =', I10,
     .      /5X,'Total number of writes        =', I10,
     .      /5X,'Total number of record passes =', I10,
     .      /5X,'Total number of record skips  =', I10)
 
      WRITE(LUNIT,9100)
      WRITE(LUNIT,9300)
 9300 FORMAT(1X,'(4) Detailed information for each Data Sub Buffer')
      WRITE(LUNIT,9310)
 9310 FORMAT(1X,
     .'+------+-------+-------+-----------------------------+',
     .'------------------+-------------------+--------+')
      WRITE(LUNIT,9312)
 9312 FORMAT(1X,
     .'!  DSB ! No of ! No of !     Data Sub Buffer Size    !',
     .'    Bank  Table   !   Element Table   ! Garbage!',/1X,
     .'!  -ID !  Read ! Write !   Alloc   !    Real   ! Ext !',
     .'Alloc! Real ! Ext ! Alloc! Real ! Ext ! Collect!')
 
      DO 300 I = 1, NOSUB
         ISSUB = NWSUB(2,I)/2
         IF( MXIDSZ(I) .LT. IDSIZE(I) ) MXIDSZ(I) = IDSIZE(I)
         IS  = MXIDSZ(I)/2
         IF( MXNBNK(I) .LT. IDSUB(4,I)) MXNBNK(I) = IDSUB(4,I)
         IF( MXNELM(I) .LT. IDSUB(7,I)) MXNELM(I) = IDSUB(7,I)
         WRITE(LUNIT,9315)
 9315    FORMAT(1X,
     .   '+------+-------+-------+-----------+-----------+-----+',
     .   '-----+------+-----+------+------+-----+--------+')
         WRITE(LUNIT,9330) IDSUB(1,I),NOREAD(I),NOWRIT(I),
     .                     ISSUB,     IS,       NODEX(I),
     .                     IDSUB(3,I),MXNBNK(I),NOBEX(I),
     .                     IDSUB(6,I),MXNELM(I),NOEEX(I),NOGAB(I)
 9330    FORMAT(1X,
     .   '!',I5,' !',2(I6,' !'),2(I10,' !'),2(I4,' !'),I5,' !',
     .   I4,' !',2(I5,' !'),I4,' !',I7,' !')
  300 CONTINUE
      WRITE(LUNIT,9310)
 
      RETURN
      END
