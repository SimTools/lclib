C**********************************************************************
      SUBROUTINE BSCAST( NN, NW )
C*****************************                                        *
C* (purpose)                                                          *
C*    For broadcast                                                   *
C*    This program has meaning only for the parallel processor.        *
C*    For the other computers, this must be dummy routine.             *
C* (Argument)                                                         *
C*    NN  : Integer data to be broadcasted                            *
C*    NW  : Number of words (I*4 unit )                               *
C* (Author)                                                           *
C*    S. Kawabata   May 1992                                          *
C**********************************************************************
 
      INTEGER NN(NW)
 
      RETURN
      END
