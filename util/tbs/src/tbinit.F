C *********************************************************************C
C*                                                                     *
C*===============================                                      *
C* Subroutine TBINIT( ID, IRET )                                       *
C*===============================                                      *
C*(Purpose)                                                            *
C*   Initialize the DSB: ID                                            *
C*(Input)                                                              *
C*   ID       : ID_Number of the DSB to be initialized.                *
C*(Used COMMON)                                                        *
C*    /TBSBNK/  For detail information, see TBALOC.                    *
C*                                                                     *
C*(Author)                                                             *
C*       S.Kawabata     June 6 '84                                     *
C*       S.Kawabata     July 20 '85    Version 3.0                     *
C*       S.Kawabata     Nov. 27 '85    Version 4.0                     *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBINIT( ID, IRET )
 
#include "tbsbnk.inc"
#include "tbsmes.inc"
#include "tbssum.inc"
 
C
      IRET  =  1
C
C  ----------------------------------------------------------------
C            Check whether the DSB: ID exist.
C  ---------------------------------------------------------------
C                         =====
         CALL TBxFSB( ID, IPSUB )
C
         IF( IPSUB .LT. 0 ) THEN
 
           WRITE(MESAGE(1),9001) ID
 9001      FORMAT(' Record ID(=',I5,' ) does not exist.')
           CALL TBxERR( 200,'TBINIT', 1)
 
           IRET = -1
           RETURN
         ENDIF
C
         IF( IDSUB(9,IPSUB) .GT. 0 ) THEN
 
           WRITE(MESAGE(1),9011)
 9011      FORMAT(' Neglect this call because of')
           WRITE(MESAGE(2),9012) ID
 9012      FORMAT(' Record ID(=',I5,' ) is shared record.')
           CALL TBxERR( 200,'TBINIT', 2)
 
           IRET = -2
           RETURN
         ENDIF
C
C  -------------------------------------------------------------------
C     Initialization of Contents of COMMON /TBSBNK/ for the IPSUB-th
C    Data_Sub_Buffer ( DSB ).
C  -------------------------------------------------------------------
 
           IF( MXNBNK(IPSUB) .LT. IDSUB(4,IPSUB))
     .          MXNBNK(IPSUB)   = IDSUB(4,IPSUB)
           IF( MXNELM(IPSUB) .LT. IDSUB(7,IPSUB))
     .          MXNELM(IPSUB)   = IDSUB(7,IPSUB)
C
           NWSUB(3,IPSUB) = 0
           IDSUB(4,IPSUB) = 0
           IEOFST         = IDSUB(5,IPSUB)
           MXELMT         = IDSUB(6,IPSUB)
           IDSUB(7,IPSUB) = 0
           IDSUB(8,IPSUB) = 0
           IDSUB(11,IPSUB)= 0
           ISTA           = IEOFST + 1
           IEND           = IEOFST + MXELMT
           DO    100   I  = ISTA, IEND
              LINK( 1, I) = -1
              LINK(3,I)   = -1
              IADTBL(1,I) = -1
  100      CONTINUE
 
           IF( MXIDSZ(IPSUB) .LT. IDSIZE(IPSUB))
     .          MXIDSZ(IPSUB)   = IDSIZE(IPSUB)
 
                IDSIZE(IPSUB)   = 0
 
 
         RETURN
         END
