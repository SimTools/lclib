C **********************************************************************
C*                                                                     *
C*============================================                         *
C* Subroutine TBNOEL(ID, BNAME, NOELM, NEARY, /MLIMSZ/)                *
C*============================================                         *
C*(Purpose)                                                            *
C*   Returns the number of element exists in the bank and their        *
C*  element number.  If specified DSB or bank does not exist           *
C*  returns NOELM -1. If DSB and bank exists but no element exists     *
C*  NOELM = 0.  If any element exists, returns number of element       *
C*  to NOELM and thier element number is returns in array NEARY.       *
C*    This routine does not check whether NOELM fits the size          *
C*  of NEARY or not.  Therefore user should prepare enough size of     *
C*  array in a calling routine.                                        *
C*    This routine does     count the element number if the number     *
C*  of words in the element less than 1.                               *
C*(Input)                                                              *
C*   ID       : DSB Identification number.                             *
C*   BNAME    : Bank_Name                                              *
C*  /MLIMSZ/  : Size of array NEARY. (optional)                        *
C*(Output)                                                             *
C*   NOELM    : Number of elements in the bank.                        *
C*       = -2  DSB does not exist .                                    *
C*       = -1  Bank does not exist .                                   *
C*       =  0  Bank exists but there is no bank element.               *
C*       = NE  The number of the bank element in the bank.             *
C*   NEARY    : Array containig the element number of the each         *
C*             element.                                                *
C*(Author)                                                             *
C*       A. Miiyamoto   June 20, 1984.                                 *
C*       A. Miiyamoto   March 20,1987.  Add /MLIMSZ/ argument.         *
C*                                                                     *
C **********************************************************************
C
#ifdef  MSP
      SUBROUTINE TBNOEL( ID, BNAME, NOELM, NEARY, /MLIMSZ/)
#endif
#ifdef  UNIX
      SUBROUTINE TBNOEL( ID, BNAME, NOELM, NEARY )
#endif
C
#include "tbsbnk.inc"
#include "tbsmes.inc"
C
      INTEGER*4     NEARY(*)
      CHARACTER*(*) BNAME
      CHARACTER*64  TNAME
 
#ifdef  UNIX
      NUMARG = 4
#endif
#ifdef  UNIX
      GO TO 9900
#endif
#ifdef  UNIX
      ENTRY  TBNOELx(ID, BNAME, NOELM, NEARY, MLIMSZ)
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
C (1) Get the pointer to the DSB
C     ----------------=======
      CALL TBxFRD( ID, IPSUB)
C     ----------------=======
      IF( IPSUB .LE. 0 ) THEN
        NOELM = -2
        RETURN
      ENDIF
 
C (2) Get the address of the Bank in Bank_Key_Table.
 
         TNAME = BNAME
 
         CALL UCUPER( LEN(BNAME), TNAME)
 
C     --------------------------===================
      CALL TBxFBK( IPSUB, TNAME, ISRT, IPBNK, IRET)
C     --------------------------===================
      IF( IRET .LT. 0 ) THEN
        NOELM = -1
        RETURN
      ENDIF
 
C (3) Get the number
 
      NOELM = KEYBNK( 2, IPBNK)
C
      IF( NOELM .EQ. 0 ) THEN
        RETURN
      ELSEIF( NOELM .LT. 0 ) THEN
        WRITE(MESAGE(1),9001) NOELM
 9001   FORMAT(' Number of elements(',I5,' ) less than 0 ')
        CALL TBxERR(600,'TBNOEL', 1)
      END IF
 
C (4) Store the element number of the each element.
 
      IF( NUMARG  .GE. 5 ) NOELM = MIN(NOELM, MLIMSZ)
 
      IPELM    = KEYBNK( 1, IPBNK)
      DO 400 I = 1, NOELM
        NEARY(I) = LINK(1, IPELM)
        IPELM    = LINK(2, IPELM)
400   CONTINUE
 
      RETURN
      END
