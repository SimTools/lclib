C**********************************************************************C
C*                                                                     *
C*=====================================
C* Subroutine TBCxGN( ID, TNAME , IRET)
C*=====================================
C*(Purpose)                                                            *
C*   Returns record name corresponding to the ID.                      *
C*(Input)                                                              *
C*   ID       : Record ID.                                             *
C*(Output)                                                             *
C*   TNAME    : Record name.
C*   IRET     : Return_Flag                                            *
C*     IRET =     1 : Normal return
C*          =    -1 : Record ID not allocated.                         *
C*(Author)                                                             *
C*   A. Miyamoto  Mar. 18 '87  Original version.                       *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBCxGN( ID, TNAME , IRET)
 
      IMPLICIT INTEGER*4 (H)
      CHARACTER*64       TNAME
#include "tbsbnk.inc"
#include "tbsmes.inc"
 
C
C=======< Entry Point >=================================================
C
C
C (2.1) Look for the ID_Number ---------------------------------------
C
      IRET = 1
      DO 200 I = 1, NOSUB
        IF( ID .EQ. IDSUB(1,I) ) THEN
          TNAME = SUBNAM(I)
          RETURN
        ENDIF
        IF( IDSUB(9,I) .LE. 0)   GO TO 200
        K = IDSUB(10,I)
        DO 210 J = 1, IDSUB(9,I)
          IF( IDSHR(J,K) .NE. ID ) GO TO 210
          TNAME  = SHRNAM( J, K)
          RETURN
210     CONTINUE
200   CONTINUE
 
      WRITE(MESAGE(1), 220) ID
220   FORMAT('Record ID=(',I5,') not exists.')
      CALL TBxERR(400, 'TBCxGN', 1)
      IRET = -1
      RETURN
C
      END
