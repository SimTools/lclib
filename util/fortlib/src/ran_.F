CC*********************************************************************C
C*                                                                     *
C*=======================                                              *
C* Function   RAN_( ISEED)                                             *
C*=======================                                              *
C*  (Purpose)                                                          *
C*     For the interface to HP RAN function
C*     Input ISEED is the seed for the randum variables.               *
C*  (Relation)                                                         *
C*     RAN is used to get uniform random numbers.                      *
C*  (Author)                                                           *
C*     A. Miyamoto            3-Dec-1984                               *
C*     A. Miyamoto          11-Oct-2001 Now RANMAR of cernlib is used. *
C*$Id$
CC**********************************************************************
C  
        FUNCTION  RAN(ISEED)

        real*4 rndm(10)
C  
#ifdef  MSP
      PRINT *,'Function RAN_ is called.'
      PRINT *,'This function sould not be called on MSP'
      STOP
#endif
#ifdef  SUN
      PRINT *,'Function RAN_ is called.'
      PRINT *,'This function sould not be called on SUN'
      STOP
#endif
#ifdef  HIUXF77
	call msgu1m(1, iseed, x, ier)
	if( ier.ne.0 ) then
	  print *,'Error detected in ran.'
	  print *,'n <1 or iseed < 0 '
	  stop
	endif
	ran = x
#endif

      call ranmar(rndm,1)
      ran = rndm(1)

      RETURN
      END
