C***********************************************************************
C*
C*========================----===
C* Subroutine SUT2PD(ITOP,IPDG)
C*========================----===
C*
C* (Purpose)
C*     Translate TOPAZ ID to PDG standard ID.
C* (Inputs)
C*        ITOP     : (I*4) :  TOPAZ ID.
C* (Output)
C*        IPDG     : (I*4) :  PDG ID.
C* (Relation)
C*     Called by:
C*     Calls no subroutines.
C* (Updaterecord)
C*     4/21/90  K.Fujii           Original version.
C*                                At present, only stable particle
C*                                codes are supported.
C*
C***********************************************************************
 
      SUBROUTINE SUT2PD(ITOP,IPDG)
 
      INTEGER*4    ITOP, IPDG
      PARAMETER  ( LN$PID = 30 )
      INTEGER*4    ITP2PD(LN$PID)
C
C                     gamma    nu_e       e   nu_mu      mu
C                    ----------------------------------------
      DATA ITP2PD  /     22,     12,     11,     14,     13,
C
C                    nu_tau     tau
C                    ----------------------------------------
     .                   16,     15,      0,      0,      0,
C
C                       pi+     pi0     eta     eta'     K+
C                    ----------------------------------------
     .                  211,    111,    221,    331,    321,
C
C                        K0      KS      KL
C                    ----------------------------------------
     .                  311,    310,    130,      0,      0,
C
C                         p       n  lambda  sigma+  sigma0
C                    ----------------------------------------
     .                 2212,   2112,   3122,   3222,   3212,
C
C                    sigma-     xi0     xi-
C                    ----------------------------------------
     .                 3112,   3322,   3312,      0,      0 /
C
C========< Entry Point >================================================
C
C--
C  Check if TOPAZ ID is in the difined range.
C--
      IT = IABS(ITOP)
      IF( IT .EQ. 130 ) THEN
        IPDG = ISIGN(3334,ITOP)
      ELSEIF ( IT.LT.1 .OR. IT.GT.LN$PID ) THEN
         PRINT *, ' >>>>> SUT2PD >>>>>'
         PRINT *, '   ITOP = ', ITOP, ' is not supported.'
         PRINT *, '   STOP.'
         STOP
      ENDIF
C--
C  Check if TOPAZ ID is supported.
C--
      IF ( ITP2PD(IT).EQ.0 ) THEN
         PRINT *, ' >>>>> SUT2PD >>>>>'
         PRINT *, '   ITOP = ', ITOP, ' is not supported.'
         PRINT *, '   STOP.'
         STOP
      ENDIF
C--
C  Set PDG ID corresponding to ITOP.
C--
      IPDG = ISIGN(ITP2PD(IT),ITOP)
C--
C  That's it.
C--
      RETURN
      END
