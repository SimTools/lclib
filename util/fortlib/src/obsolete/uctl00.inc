C**********************************************************************
C* 
C*  Common to control dynammic allocation of CTL tape.
C* 
C**********************************************************************
C* 
      CHARACTER*6 MOVOL
      COMMON /UCTL00/ MOUNIT, MOPOS, MOVOL
C* 
C*    MOUNIT : Unit # currently in allocation.
C*             Set to zero at the first call to UCTLAL or call to
C*             UCTLFR.
C*    MOPOS  : Data set position.
C*    MOVOL  : Vol=ser
C* 
