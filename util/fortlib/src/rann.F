CC*********************************************************************C
C*                                                                     *
C*=======================                                              *
C* Function   RANN( ISEED)                                             *
C*=======================                                              *
C*  (Purpose)                                                          *
C*     Function to generate normal random number with                  *
C*       m      :  mean value = 0.0                                    *
C*       sigma  :  s.d.       = 1.0.                                   *
C*     Input ISEED is the seed for the randum variables.               *
C*  (Relation)                                                         *
C*     RAN is used to get uniform random numbers.                      *
C*  (Author)                                                           *
C*     A. Miyamoto            3-Dec-1984                               *
C*     A. Miyamoto           11-Oct-2001  RNORML of CERNLIB is used    *
C*$Id$
CC**********************************************************************
C  
      FUNCTION  RANN(ISEED)
      real*4  val(10)
      call rnorml( val, 1)
      rann=val(1)

      RETURN
      END
