CC*********************************************************************C
C*                                                                     *
C*--------------------==============                                   *
C* SUBROUTINE UWSWAP( IBUFF, LENGTH)                                   *
C*--------------------==============                                   *
C*  (Purpose)                                                          *
C*       Do word swap.                                                 *
C*                                                                     *
C*  (Input)                                                            *
C*       IBUFF  : Data buffer.                                         *
C*       LENGTH : Length of data.                                      *
C*                                                                     *
C*  (Output)                                                           *
C*       IBUFF  : Data after word swap.                                *
C*                                                                     *
C*  (Author)                                                           *
C*       A. Miyamoto   24-Jan-1986  A. Miyamoto                        *
C*                                                                     *
C***********************************************************************
   
      SUBROUTINE UWSWAP( IBUFF, LENGTH)
   
      INTEGER*2    IBUFF(*)
      INTEGER*2    KTEMP
C  
C======< Entry Point >==================================================
C  
C  
   
      DO 100 IAD=2, 2*LENGTH, 2
         KTEMP  = IBUFF(IAD)
         IBUFF(IAD)   = IBUFF(IAD-1)
         IBUFF(IAD-1) = KTEMP
  100 CONTINUE
   
      RETURN
      END
