C *********************************************************************C
C*                                                                     *
C*==========================                                           *
C* Subroutine TBLLNK                                                   *
C*==========================                                           *
C*(Purpose)                                                            *
C*   Dump the Link_Table and Address_table.                            *
C*(Input)                                                              *
C*   none                                                              *
C*(Used COMMON)                                                        *
C*   /TBSBNK/                                                          *
C*(Author)                                                             *
C*      A.Miyamoto   June 12 '84                                       *
C *********************************************************************C
 
      SUBROUTINE TBLLNK
 
#include "tbsbnk.inc"
#include "tbsbuf.inc"
#include "tbsprm.inc"
#include "tbsmes.inc"
C
C     CHARACTER*64 TNAME
C
C (1) if NOSUB le 0 then return
C
      IF( NOSUB .LE. 0 ) THEN
 
           WRITE(MESAGE(1),9001)
 9001      FORMAT(' No Data Sub-buffer is defined.')
           CALL TBxERR( 200,'TBLLNK', 1)
 
        RETURN
      END IF
C
C (2) Dump Link table and address table
C
      WRITE(LUNIT,100)
100   FORMAT(//,' *** TBS dump, Link_table and Address_table ')
      DO 200 I=1, NOSUB
        WRITE(LUNIT,210) I, IDSUB(1,I), SUBNAM(I)
210     FORMAT(' ---',I2,'--- DSB ID=',I5,'  DSB name =',A32)
        WRITE(LUNIT,220)
220     FORMAT(7X,' LINK(i,j)           IADTBL(i,j)     NWELM(i,j)',/,
     .         7X,'(1,j) (2,j) (3,j) (1,j) (2,j) (3,j) (1,j) (2,j)',
     .               '   Elmnt#   Bank_Name')
        DO 300 J=1,IDSUB(4,I)
          JB    = IDSUB(2,I) + J
          NOEL  = KEYBNK(2,JB)
          IF( NOEL .GT. 0 ) THEN
             JP       = KEYBNK(1,JB)
             DO 400 L = 1, NOEL
               JO     = LINK(2,JP)
               WRITE(LUNIT,310) JP, LINK(1,JP), LINK(2,JP), LINK(3,JP),
     .         (IADTBL(N,JP),N=1,3),(NWELM(M,JP),M=1,2),
     .         LINK(1,JP),BNKNAM(JB)
310            FORMAT(5X, I4, I3, 8I6,1X,A64)
               JP     = JO
 400         CONTINUE
          ELSE
             WRITE(LUNIT,320) BNKNAM(JB)
 320         FORMAT(10X,' Not used Bank ',32X,A64)
           ENDIF
300     CONTINUE
200   CONTINUE
 
      RETURN
      END
