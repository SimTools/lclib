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
C*                                                                     *
CC**********************************************************************
C  
#if defined(MSP) || defined(SUN) || defined(HIUXF77) || defined(AIX)
	FUNCTION  RANN(ISEED)
#endif
#if defined(HP) || defined(MACHTEN_PPC)
	  FUNCTION  RANN_(ISEED)
#endif
C  
#if defined(MSP) || defined(SUN) || defined(HIUXF77) || defined(AIX)
       RANN = (RAN(ISEED) + RAN(ISEED) + RAN(ISEED) +
#endif
#if defined(HP) || defined(MACHTEN_PPC)
	RANN_ = (RAN(ISEED) + RAN(ISEED) + RAN(ISEED) +
#endif
     .        RAN(ISEED) + RAN(ISEED) + RAN(ISEED) +
     .        RAN(ISEED) + RAN(ISEED) + RAN(ISEED) +
     .        RAN(ISEED) + RAN(ISEED) + RAN(ISEED) - 6.0E0 )
      RETURN
      END
