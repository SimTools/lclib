C**********************************************************************C
C*                                                                     *
C*==========================                                           *
C* Subroutine TBLBNM                                                   *
C*==========================                                           *
C*(Purpose)                                                            *
C*   Dump the Sort_table, Bank_name_table and Bank_key_table.          *
C*(Input)                                                              *
C*   none                                                              *
C*(Used COMMON)                                                        *
C*  /TBSBNK/                                                           *
C*(Author)                                                             *
C*       A.Miyamoto     June 12 '84                                    *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBLBNM
 
#include "tbsbnk.inc"
#include "tbsprm.inc"
#include "tbsmes.inc"
 
C (1) Return if NOSUB .le. 0
 
      IF( NOSUB. LE. 0 ) THEN
 
           WRITE(MESAGE(1),9001)
 9001      FORMAT(' No Data Sub-buffer is defined.')
           CALL TBxERR( 200,'TBLBNM', 1)
 
        RETURN
      END IF
 
C (2) Dump Sort_table, Bank_key_table and Bank_name_table.
 
      WRITE(LUNIT,100)
100   FORMAT(//,' *** TBS dump, Sort_table, Bank_key_table and ',
     >          ' Bank_name_table ***')
      DO 200 I=1, NOSUB
        WRITE(LUNIT,210) I, IDSUB(1,I), SUBNAM(I)
210     FORMAT(' ---',I2,'--- DSB ID=',I5,'  DSB name =',A64)
         IF( IDSUB(9,I) .GT. 0 .AND. IDSUB(11,I) .GT. 0 ) THEN
             IASH  = IDSUB(11,I)
             ISSH  = IDSUB(10,I)
             WRITE(LUNIT,212) SHRNAM( IASH, ISSH)
212          FORMAT(1X,'                  Record name(',A64,')')
         ENDIF
        WRITE(LUNIT,220)
220     FORMAT(10X,
     >      '            NWBNK           KEYBNK            ',/,
     >  10X,'ISORT   (1,i)   (2,i)   (1,i)   (2,i)     Bank_name ')
        DO 300 J=1,IDSUB(4,I)
          JP = IDSUB(2,I) + J
          WRITE(LUNIT,310) JP, ISORT(JP), (NWBNK(I1,JP),I1=1,2),
     .                 (KEYBNK(I2,JP),I2=1,2),BNKNAM(JP)
310       FORMAT(1X,   I5,  5(2X,I6),                  3X,    A64)
300     CONTINUE
200   CONTINUE
 
      RETURN
      END
