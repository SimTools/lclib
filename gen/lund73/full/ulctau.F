CC**********************************************************************
C*
C*=======================
C* Function ULCTAU(KF)
C*=======================
C*
C* (Function)
C*    Gives ctau in cm for IDPDG=KF.
C* (Inputs)
C*    KF           :(I*4): PDG particle ID code.
C* (Outputs)
C*    ULCTAU       :(R*4): ctau (cm).
C* (Update Records)
C*   95/02/10  K.Fujii		Original version.
C*				This version resets ctau less than
C*
CC**********************************************************************
 
      REAL FUNCTION ULCTAU(KF)

      INTEGER * 4  KF
C--
C  LUND commons.
C--
      COMMON /LUDAT2/  KCHG(500,3), PMAS(500,4), PARF(2000), VCKM(4,4)
      INTEGER*4        KCHG
      REAL   *4        PMAS, PARF, VCKM
C--
C  Constants
C--
      EXTERNAL         LUDATA
C
C ======< Entry Point >=============================================
C
C--
C  Get Compressed ID.
C--
      KC = LUCOMP(KF)
C--
C  Get ctau.
C--
      ULCTAU = PMAS(KC,4)/10.
C--
C  That's it.
C--
      RETURN
      END
