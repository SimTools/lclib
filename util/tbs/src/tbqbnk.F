C*********************************************************************
C*
C* --------------------------===============================
C* Subroutine TBQBNK( IDDSB, DSBNAM, NOBNK, BNAME, /MLIMSZ/)
C* --------------------------===============================
C*
C*(Function)
C*   Query DSB name, nomber of bank and bank name for
C*
C*(Input)
C*   IDDSB  ...   DSB identification number.
C*  /MLIMSZ/  : Size of array BNAME. (optional)
C*
C*(Output)
C*   DSBNAM ...   DSB name, CHARACTER*64
C*   NOBNK  ...   Number of bank on the DSB.
C*   BNAME  ...   Array of Character*64, containing the bank name on the
C*                DSB.
C*
C*(Author)
C*   A. Miyamoto    13-Feb-1985
C*   S.Kawabata     29-Nov-1985    V4.0
C*   A.Miyamoto     20-Mar-1987    Add /MLIMSZ/ argument.
C*
C*********************************************************************
C*
#ifdef  MSP
      SUBROUTINE TBQBNK( IDDSB, DSBNAM, NOBNK, BNAME, /MLIMSZ/ )
#endif
#ifdef  UNIX
      SUBROUTINE TBQBNK( IDDSB, DSBNAM, NOBNK, BNAME )
#endif
C
#include "tbsbnk.inc"
#include "tbsmes.inc"
C
      CHARACTER*64  DSBNAM, BNAME(*)
 
C
#ifdef  UNIX
      NUMARG = 4
#endif
#ifdef  UNIX
      GO TO 9900
#endif
#ifdef  UNIX
      ENTRY TBQBNKx(IDDSB, DSBNAM, NOBNK, BNAME, MLIMSZ )
#endif
#ifdef  UNIX
      NUMARG = 5
#endif
#ifdef  UNIX
9900  CONTINUE
#endif
#ifdef  MSP
       NUMARG = NARG(0)
#endif
C ==================================================================
C   1.Get  DSB name.
C ==================================================================
 
      CALL TBxFRD( IDDSB, IPSUB )
      IF(IPSUB.LT.0) THEN
 
           WRITE(MESAGE(1),9001) IDDSB
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBQBNK', 1)
 
        NOBNK = 0
        RETURN
      ENDIF
 
C ==================================================================
C  2. Get DSB name, Nomber of bank and bank names.
C ==================================================================
 
      DSBNAM = SUBNAM(IPSUB)
      IASH  = IDSUB(11,IPSUB)
      IF( IASH .GT. 0 ) THEN
          ISSH  = IDSUB(10,IPSUB)
          DSBNAM = SHRNAM( IASH, ISSH)
      ENDIF
 
      NOBNK  = IDSUB( 4, IPSUB)
      IF( NUMARG  .GE. 5) NOBNK = MIN(NOBNK, MLIMSZ)
      IP     = IDSUB(2, IPSUB) + 1
      NW     = LEN(BNKNAM(IP))*NOBNK/4
 
      CALL UVCOPY( NW, BNKNAM(IP), BNAME(1))
 
C   The following statements are for the versions before V402.
C     NOBNK  = IDSUB( 4, IPSUB)
C     DO 200 I = 1, NOBNK
C        IP  = IDSUB(2, IPSUB) + I
C        BNAME(I) = BNKNAM(IP)
C200  CONTINUE
 
      RETURN
C
      END
