C***********************************************************************
C*=======================                                              *
      SUBROUTINE DRLOOP( NLOOP )
C*=======================                                              *
C*((Purpose))                                                          *
C*    Dummy generation of the random numbers for the parallel processor*
C*    This program has meaning only for the parallel processor.        *
C*    For the other computers, this must be dummy routine.             *
C*((Author))                                                           *
C*    S.Kawabata  Nov. '91 at KEK                                      *
C***********************************************************************
      DO 100 I = 1, NLOOP
         DUMMY = DRN(DUM)
  100 CONTINUE
      RETURN
      END
