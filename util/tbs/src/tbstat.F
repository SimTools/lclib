C***********************************************************************
C*                                                                     *
C*==============================================                       *
C* Subroutine TBSTAT( ID, NOBANK, NOELM, NFREE )                       *
C*==============================================                       *
C*(Purpose)                                                            *
C*   Get the status of the DSB ID.  Returns the created number of      *
C*  bank NOBANK, the total number of element in the DSB NOELM and      *
C*  the number of free space in the DSB NFREE.  NFREE includes the     *
C*  the space freeed after the deletion of the bank and/or the bank    *
C*  element.  Notice that to the maximum length of the data you        *
C*  can store more is the NFREE-2, since two words space will be       *
C*  occupied by the bank element header when you put the data.         *
C*(Input )                                                             *
C*   ID       : DSB ID                                                 *
C*(Output)                                                             *
C*   NOBANK   : Number of created bank.                                *
C*             If specified DSB does not exist, NOBANK = -1            *
C*   NOELM    : Total number of element in this DSB.                   *
C*   NFREE    : Free space in DSB, including the area made available   *
C*             after the deletion of the bank or the bank element.     *
C*(Author)                                                             *
C*       A. Miiyamoto   June 21, 1984.                                 *
C*                                                                     *
C***********************************************************************
 
      SUBROUTINE TBSTAT( ID, NOBANK, NOELM, NFREE )
 
      IMPLICIT INTEGER*4 (H)
#include "tbsbnk.inc"
C
C
C*(1) Get the pointer to the DSB
C*
C     -----------------=======
      CALL TBxFRD( ID, IPSUB)
C     -----------------=======
      IF( IPSUB .LE. 0 ) THEN
        NOBANK = -1
        RETURN
      ENDIF
C*
C*(2) Get the number of Bank and the number of elements.
C*
      NOBANK = IDSUB( 4, IPSUB)
      NOELM  = IDSUB( 7, IPSUB)
      HFREE  = NWSUB( 2, IPSUB) - NWSUB( 3, IPSUB)
      NFREE  = (HFREE+1)/2
C*
C*(3)  Count the free space in the DSB if Garbage collection flag is
C*    set, otherwise RETURN immediately.
C*
      IF( IDSUB( 8, IPSUB) .EQ. 0 ) RETURN
      HWDSB = NWSUB( 2, IPSUB)
      NWDSB = (HWDSB+ 1)/2
C
C*  A. Loop over existing Bank to count the space occupied by the
C*    Bank element.
C
      DO 300 IBNK = 1, NOBANK
C
        IPELM  = KEYBNK( 1, IBNK)
        IELMNO = KEYBNK( 2, IBNK)
        IF( IELMNO .LE. 0 ) GO TO 300
C*
C*  B.  Loop over the bank element to get the size of the each bank
C*     element.
C*
        DO 350 I = 1, IELMNO
          HWDSB  = HWDSB - NWELM(1,IPELM)
          IPELM  = LINK(2, IPELM)
350     CONTINUE
C
300   CONTINUE
      NFREE      = (HWDSB+1)/2
C*
C*(5)  Termination.
C*
      RETURN
      END
