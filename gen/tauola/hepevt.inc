      PARAMETER ( NMXHEP = 2000 )
      COMMON /HEPEVT/ NEVHEP, NHEP, ISTHEP(NMXHEP), IDHEP(NMXHEP),
     .                JMOHEP(2,NMXHEP), JDAHEP(2,NMXHEP),
     .                PHEP(5,NMXHEP), VHEP(4,NMXHEP)
#if !defined(JETSET73)
      REAL*8          PHEP, VHEP
#else
      REAL*4          PHEP, VHEP
#endif

