CC*********************************************************************C
C*                                                                     *
C*====================                                                 *
C* SUBROUTINE UINIMZ (IMODE, FUNC, NTERMS, PARAM, IPRINT, DELTAA,      *
C*   %                   SIGMAA, CHISQR, KAISU)                        *
C*====================                                                 *
C*                                                                     *
C*  (Purpose)                                                          *
C*       Main routine to minimize a function in a multi-parameter      *
C*       space.                                                        *
C       85. 6. 7.   T. ISHII
C*                                                                     *
C*(Input Parameters)                                                   *
C*      IMODE   - Determines method of minimizing the function.        *
C*             1  uses MRADLS in the minimizing stage which is based on*
C*                GRADLS of the Bevington library.                     *
C*             2  uses MHIFIT in the minimizing stage which is based on*
C*                CHIFIT of the Bevington library.                     *
C*           Else uses MURFIT in the minimizing stage which is based on*
C*                CURFIT of the Bevington library.                     *
C*      FUNC    - Name of the function to be minimized.                *
C*                This routine must be declaired as EXTERNAL in        *
C*                the caller routine.  Calling sequence is,            *
C*                                                                     *
C*               Function FUNC(NTERMS,PARAM)                           *
C*                                                                     *
C*               Inputs to function FUNC are,                          *
C*                                                                     *
C*               NTERMS ... Number of parameters.                      *
C*               PARAM  ... Array of parameters.                       *
C*                                                                     *
C*               Output is a value of function FUNC.                   *
C*               For example of this routine, please reffer to function*
C*               CIRCX2.                                               *
C*      NTERMS  - Number of parameters                                 *
C*      PARAM   - Array of parameters.                                 *
C*      IPRINT  - Flag to print out the result at every call to MURFIT.*
C*      DELTAA  - Array of increments for parameters PARAM.            *
C*                                                                     *
C*(Output Parameters)                                                  *
C*      PARAM   - Array of parameters, input and output.               *
C*      SIGMAA  - Array of standard deviations for parameters PARAM.   *
C*      CHISQR  - Reduced CHI squre for the fit.                       *
C*      KAISU   - Calling number to MRADLS, MHIFIT or MURFIT.          *
C*                                                                     *
C*                                                                     *
C*  (Author)                                                           *
C*       T. Ishii (INS : Tel 0424 (61) 4131 - X326)                    *
C*                                                                     *
C*  (Date)                                                             *
C*       June 7, 1985                                                  *
C*       June 8, 1986  Added IPRINT in the arguments.                  *
C*                                                                     *
CC**********************************************************************
C  
      SUBROUTINE UINIMZ (IMODE, FUNC, NTERMS, PARAM, IPRINT, DELTAA,
     %                   SIGMAA, CHISQR, KAISU)
C  
C  MAIN ROUTINE TO MINIMIZE A FUNCTION.
C       85. 6. 7.   T. ISHII
C  
      DIMENSION PARAM(1),DELTAA(1),SIGMAA(1)
      EXTERNAL FUNC
      DATA DCHMAX / 1. E-3 /
      DATA CCHMAX / 1. E-4 /
C  
      FLAMDA = 0.001
C  
      CHISQ2 = FUNC(NTERMS,PARAM)
      IF(IPRINT.NE.0) THEN
        WRITE(6,6021) CHISQ2,(PARAM(I),I = 1,NTERMS)
        WRITE(6,6023)       (DELTAA(I),I = 1,NTERMS)
 6023   FORMAT ( 1H ,5X,12X    ,'DELTA =',1X,11(1PE10.2) )
      ENDIF
      KAISU  = 1
C  
  200 CONTINUE
      CHISQ1 = CHISQ2
      IF(IMODE.EQ.1) THEN
        CALL URADLS (FUNC, NTERMS, PARAM, DELTAA, CHISQ2)
      ELSE IF(IMODE.EQ.2) THEN
        CALL UHIFIT (FUNC, NTERMS, PARAM, DELTAA, SIGMAA, CHISQ2)
      ELSE
        CALL UURFIT(FUNC, NTERMS, PARAM, DELTAA, FLAMDA,SIGMAA,CHISQ2)
      ENDIF
      IF(IPRINT.NE.0) THEN
        WRITE(6,6021) CHISQ2,(PARAM(I),I = 1,NTERMS)
 6021   FORMAT ( 1H ,5X,'CHISQ =',1PE12.4,1X,11(1PE10.2) )
        WRITE(6,6022)       (SIGMAA(I),I = 1,NTERMS)
 6022   FORMAT ( 1H ,5X,12X    ,'SIGMA =',1X,11(1PE10.2) )
      ENDIF
      KAISU  = KAISU + 1
      IF(KAISU.GT.19) GO TO 300
      IF(ABS(CHISQ2).LT.ABS(CCHMAX)) GO TO 300
      IF(ABS(CHISQ1 - CHISQ2).GT.ABS(DCHMAX*CHISQ1)) GO TO 200
C  
  300 CONTINUE
      CHISQR = CHISQ2
      RETURN
      END
