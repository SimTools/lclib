#include "nmxhep.inc"
#define HEPEVT HEP_EVT
      COMMON /HEPEVT/ NEVHEP, NHEP, ISTHEP(NMXHEP), IDHEP(NMXHEP),
     .                JMOHEP(2,NMXHEP), JDAHEP(2,NMXHEP),
     .                PHEP(5,NMXHEP), VHEP(4,NMXHEP)
C#if !defined(JETSET73)
      REAL*8          PHEP, VHEP
C#else
C      REAL*4          PHEP, VHEP
C#endif
