C***********************************************************************
C*
C*===============================
C* Subroutine LUTUHL(MODE,IHLON)
C*========================-----==
C*
C* (Function)
C*    I/O interface for tau polarization switch.
C* (Inputs)
C*    MODE  : (I*4) : (1,2)=(put,get)
C*    IHLON : (I*4) : (0,1)=(off,on)
C* (Output)
C*    IHLON : (I*4) : (0,1)=(off,on)
C* (Update Record)
C*   95/02/11  K.Ffujii		Original version.
C*
C***********************************************************************

      SUBROUTINE LUTUHL(MODE,IHLON)

      INTEGER*4 MODE, IHLON
      INTEGER*4 IHLSAV
      SAVE IHLSAV
C
C========< Entry Point >================================================
C
C--
C  Set IHLSAV.
C--
      IF ( MODE.EQ.1 ) THEN
         IHLSAV = IHLON
         PRINT *, ' LUTUHL sets IHLON = ', IHLON
      ELSE
         IHLON = IHLSAV
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
