C**********************************************************************
      SUBROUTINE BSDSUM( DD, NW )
C*****************************                                        *
C* (purpose)                                                          *
C*   Broadcast for a parallel processor                               *
C*    This program has meaning only for the parallel processor.        *
C*    For the other computers, this must be dummy routine.             *
C* (Argument)                                                         *
C*    NN  : Integer data to be broadcasted                            *
C*    NW  : Number of words (I*4 unit )                               *
C* (Author)                                                           *
C*    S. Kawabata   May 1992                                          *
C**********************************************************************
      REAL*8  DD(NW)
      PARAMETER (ISIZE = 65536)
      COMMON /BSWORK/ IWORK(ISIZE)
*     REAL*8 WORK(ISIZE/2)
*     EQUIVALENCE (IWORK(1),WORK(1))
*#ifdef INTEL
*             call gdsum( DD, NOWORK, WORK)
*#elif AP
*             do 460 I = 1 , NDMX
*             do 460 J = 1 , NDIM
*             call c2dsum(D(I,J),DTMP,IR)
*             D(I,J) = DTMP
* 460         continue
*#elif NCUBE
*             call dsumrn( D , NOWORK)
*#endif
      RETURN
      END
