C*********************************************************************
C*
C* ---------------------------------=======
C* Subroutine TBQEWH( ID, BNAME, NE, NWORDS)
C* ---------------------------------=======
C*
C*(Function)
C*   Returns a number of INTEGER*2 word in a bank element.
C*
C*(Input)
C*   ID     ...   DSB identification number.
C*   BNAME  ...   Bank Name.
C*   NE     ...   Element number.
C*
C*(Output)
C*   NWORDS ...   Number of integer*2 words in a bank element. Note that
C*                NOWRDS is a INTEGER*4 variable.
C*         = 0    when specified element not exist.
C*         = -1   when bank not exist.
C*         = -2   when DSB ID not exist.
C*
C*(Author)
C*   A. Miyamoto    22-Jul-1985
C*
C*********************************************************************
C*
      SUBROUTINE TBQEWH( ID, BNAME, NE, NWORDS)
C
#include "tbsbnk.inc"
#include "tbsmes.inc"
C
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
C    1.1  Convert to the upper case character.
 
      LBNAM = LEN(BNAME)
      TNAME = BNAME
      CALL UCUPER( LBNAM, TNAME)
 
C (2) Get IPSUB.. Pointer to DSB.
 
C     -----------------========
      CALL TBxFRD ( ID, IPSUB )
C     -----------------========
      IF ( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBQEWH', 1)
 
        NWORDS = -1
        RETURN
      ENDIF
 
C (3) Get IPBNK, and ISRT
 
C     ---------------------------====================
      CALL TBxFBK ( IPSUB, TNAME, ISRT, IPBNK, IRET )
C     ---------------------------====================
 
      IF ( IRET  .LT. 0 ) THEN
 
             WRITE(MESAGE(1),9011)
 9011        FORMAT(' The following bank does not exist.')
             WRITE(MESAGE(2),9012) TNAME
 9012        FORMAT(' ',A64)
             CALL TBxERR( 200,'TBQEWH', 2)
 
        NWORDS = -2
        RETURN
      ENDIF
 
C (4) Get IPELM, pointer to element.
 
C     ------------------------=============
      CALL TBxFEL ( IPBNK, NE, IPELM, IRET )
C     ------------------------=============
      IF ( IRET  .LT. 0 ) THEN
        NWORDS = 0
        RETURN
      ENDIF
 
C (5) Returns number of words in element.
 
      NWORDS = NWELM(2,IPELM)
      RETURN
      END
