C**********************************************************************
C*
C* Common /PRUNLS/
C* contains RUNLIST information in the data base.
C* These common is loaded by PRGCNS or PRINFO, through the call of
C* subroutine PRLRNL
C*
C**********************************************************************
      PARAMETER      (MXxRLW=200)
      PARAMETER      (MXxRCD=100)
      COMMON /PRUNLS/ NWRNL, LRUNLS(MXxRLW),
     >                NBWORD(MXxRCD), NBRNLS(MXxRLW,MXxRCD)
C=    DATA            LRUNLS/MXxRLW*-999/
