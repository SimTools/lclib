C*********************************************************************
C*
C*  Common /PRTRIG/
C*
C*  Unpaked trigger list.
C*
C*********************************************************************
C
      COMMON /PRTRIG/ NTIDRP(64), NTTOFH(32), XTCTIM
C
C*    NTIDRP(i) ; IDC R-Phi trigger level(1 to 3) for i-th sector.
C*    NTTOFH(i) ; TOF trigger bit (0 or 1)
C*    XTCTIM    ; C-trigger timing. (nsec)
C*
