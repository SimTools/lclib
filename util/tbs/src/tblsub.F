C *********************************************************************C
C*                                                                     *
C*==========================                                           *
C* Subroutine TBLSUB                                                   *
C*==========================                                           *
C*(Purpose)                                                            *
C*   Dump the TBS parameters, such as MAXSIZ, NWFREE, NOSUB,           *
C*  SUBNAM, NWSUB, and IDSUB.                                          *
C*(Input)                                                              *
C*   none                                                              *
C*(Used COMMON)                                                        *
C*  /TBSBNK/                                                           *
C*(Author)                                                             *
C*       A.Miyamoto     June 12 '84                                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLSUB
 
#include "tbsbnk.inc"
#include "tbsprm.inc"
      INTEGER NSUB(3)
      CHARACTER*64 TNAME
      CHARACTER*2  BLNK,STAR,MARK
      DATA  BLNK,STAR/ '  ','ON'/
 
C     Dump MAXSUB, MAXBNK, MAXELM, MAXSIZ, NWFREE, and NOSUB
 
      NMAXS   = MAXSIZ/2
      NFREE   = NWFREE/2
 
      WRITE(LUNIT,100)
100   FORMAT(1H1,' *** Tristan_Bank_System ',
     >   ' Sub_buffer parameter dump ( sub. TBLSUB )****')
      WRITE(LUNIT,110) MAXSUB,MAXBNK,MAXELM,NMAXS,NFREE,NOSUB,NOSHR
110   FORMAT(5X,
     >    ' Maximum allocatable Sub_Buffer number(MAXSUB) =',I8,/,
     > 5X,' Maximum allocatable bank number      (MAXBNK) =',I8,/,
     > 5X,' Maximum allocatable element number   (MAXELM) =',I8,/,
     > 5X,' Allocated TBS buffer size            (MAXSIZ) =',I8,/,
     > 5X,' Unused TBS buffer size               (NWFREE) =',I8,/,
     > 5X,' Defined Sub_Buffer number            (NOSUB)  =',I8,/,
     > 5X,' Number of shared DSBs                (NOSHR)  =',I8)
C
C     Dump SUBNAM, NWSUB, and IDSUB
C
      IF(NOSUB.LE.0) GO TO 9000
 
      WRITE(LUNIT,9203)
 9203 FORMAT(//1H )
      WRITE(LUNIT,9205)
 9205 FORMAT(1X,
     .       6('----------'),'---+--+',6('----------'),'----')
      WRITE(LUNIT,9200)
 9200 FORMAT(22X,'Data Sub Buffer Name',25X,
     .                    26X,'Record name',27X)
      WRITE(LUNIT,9205)
 
      DO 250 I = 1, NOSUB
         MARK = BLNK
         IF( IDSUB(9,I) .LE. 0 ) THEN
             MARK = STAR
         ELSEIF( IDSUB(11,I) .LE. 0 ) THEN
             MARK = STAR
         ENDIF
         WRITE(LUNIT,9210) SUBNAM(I),MARK,SUBNAM(I)
 9210       FORMAT(1X,A64,A2,' ',A64)
         DO 200 J = 1, IDSUB(9,I)
            K     = IDSUB(10,I)
            MARK  = BLNK
            IF( J .EQ. IDSUB(11,I)) MARK = STAR
            WRITE(LUNIT,9220) MARK,SHRNAM(J,K)
 9220       FORMAT(1X,64X,A2,' ',A64)
  200    CONTINUE
         WRITE(LUNIT,9205)
  250 CONTINUE
 
 
      WRITE(LUNIT,9300)
 9300 FORMAT(//1H ,
     .'DSB       NWSUB       Rec               IDSUB(i,j)',/1X,
     .'-ID (1,j) (2,j) (3,j) -ID   2   3   4   5   6   7   8  9 10 11',
     .20X,'Record name')
      WRITE(LUNIT,9305)
 9305 FORMAT(1X,
     .'--+-----+-----+-----+---+---+---+---+---+---+---+---+--+--+--',
     .'+',6('----------'),'-----')
 
      DO 400 I = 1, NOSUB
         IF( IDSUB(9,I) .GT. 0 .AND. IDSUB(11,I) .GT. 0 ) THEN
             IASH  = IDSUB(11,I)
             ISSH  = IDSUB(10,I)
             TNAME = SHRNAM( IASH, ISSH)
             IDREC = IDSHR( IASH, ISSH)
         ELSE
             TNAME = SUBNAM( I )
             IDREC = IDSUB(1,I)
         ENDIF
         DO 300 K = 1,3
            NSUB(K) = NWSUB(K,I)/2
  300    CONTINUE
         WRITE(LUNIT,9310) IDSUB(1,I),(NSUB(K),K=1,3),IDREC,
     .                    (IDSUB(I1,I),I1=2,11),TNAME
 9310    FORMAT(1X,I3,3I6,8I4,3I3,1X,A64)
400   CONTINUE
      WRITE(LUNIT,9305)
C*
C*(3) Termination
C*
9000  CONTINUE
      RETURN
      END
