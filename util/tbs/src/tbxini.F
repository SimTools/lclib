C *********************************************************************C
C*                                                                     *
C*===============================                                      *
C* Subroutine TBxINI( ID, IRET )                                       *
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
 
      SUBROUTINE TBxINI( ID, IRET )
 
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
 9001        FORMAT(' Record ID(=',I5,' ) does not exist.')
             CALL TBxERR( 200,'TBxINI', 1)
             IRET = -1
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
 
           NWSUB(3,IPSUB) = 0
           IDSUB(4,IPSUB) = 0
           IEOFST         = IDSUB(5,IPSUB)
           MXELMT         = IDSUB(6,IPSUB)
           IDSUB(7,IPSUB) = 0
           IDSUB(8,IPSUB) = 0
           ISTA           = IEOFST + 1
           IEND           = IEOFST + MXELMT
           DO    100   J  = ISTA, IEND
              LINK( 1, J) = -1
              LINK(3,J)   = -1
              IADTBL(1,J) = -1
              DO 100   I  = 1, 2
                 NWELM(I,J) = -1
  100      CONTINUE
 
           IF( MXIDSZ(IPSUB) .LT. IDSIZE(IPSUB))
     .          MXIDSZ(IPSUB)   = IDSIZE(IPSUB)
 
                IDSIZE(IPSUB)   = 0
 
 
 
 
         RETURN
         END
