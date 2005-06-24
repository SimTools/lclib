C***********************************************************************
C*
C*=================================
C* Subroutine SMRRIN(LEVEL,IDEBUG)
C*=================================
C*
C* (Purpose)
C*     Run initialization for searing module.
C* (Inputs)
C*     LEVEL   :(I*4): level flag.
C*     IDEBUG  :(I*4): debug flag.
C* (Output) None.
C* (Relation)
C*     Called by steering.
C*     Calls no subroutines.
C* (Updaterecord)
C*    04/18/90  K.Fujii           Original version.
C*    05/25/90  K.Fujii           Input stream for tracking device
C*                                parameters has been changed.
C*    04/04/91  K.Fujii           Added vertex detector parameters.
C*    08/31/95  K.Fujii           Allow run continuation.
C*    09/01/95  K.Fujii           ISEED now saved as an integer.
C*
C***********************************************************************
 
      SUBROUTINE SMRRIN(LEVEL,IDEBUG)
 
#include "smrvgo.inc"
#include "smrtgo.inc"
#include "smrego.inc"
#include "smrhgo.inc"
#include "smrres.inc"
#include "smrrnd.inc"
#include "smrfld.inc"
#include "prfeld.inc"
#include "smrclp.inc"
      PARAMETER      ( LNxPRM = 200 )
      INTEGER*4       IMRPRM(LNxPRM)
      REAL   *4       SMRPRM(LNxPRM)
      EQUIVALENCE    (IMRPRM,SMRPRM)
C--
      CHARACTER*64    BKNAM1 / 'Swimmer'  /
      CHARACTER*64    BKNAM2 / 'Smearing' /
C
C========< Entry Point >================================================
C
#ifdef HIUXF77
      CALL SMRBLK
#endif
C--
C  Check if BFIELD is consistently defined.
C--
      IF ( IMODBF(1).NE.0 .AND. IMODBF(2).NE.0 .AND.
     .     BFDATA(1).NE.BFDATA(2) ) THEN
         PRINT *, ' >>>>> Error in SMRRIN >>>>>>> '
         PRINT *, '   BFIELD doubly defined with different values.'
         PRINT *, '   STOP.'
         PRINT *, '   Set BFIELD only once for either SWIMMER '
         PRINT *, '   or SMEARING. '
      ENDIF
C--
C  Get B field value used in 'Swimmer'.
C--
      CALL TBGET(2,BKNAM1,2,NW,SMRPRM(1),IRT)
      IF ( IRT.GE.0 ) THEN
         IF ( IMODBF(2).NE.0 ) THEN
            PRINT *, ' '
            PRINT *, ' >>>>> Caution from SMRRIN >>>>>> '
            PRINT *, '   Smearing will use BFIELD passed from Swimmer.'
            PRINT *, '       BFIELD = ', SMRPRM(2),' kG'
         ENDIF
         BSWIM = SMRPRM(2)
         IF ( BSWIM.LT.1.E-2 ) THEN
            PRINT *, '     BFIELD = ', BSWIM, ' is too small.'
            PRINT *, '     Will use BFIELD = 0.01 kG.'
            BSWIM = 1.E-2
         ENDIF
         PTOR   = PTOR*BFIELD/BSWIM
         ROPT   = 1/PTOR
         BFIELD = BSWIM
      ENDIF
C--
C  Detector information to be saved in 'Smearing'
C--
      CALL TBCRTE(2,BKNAM2,0,0,IRT)
C--
C  TBGET smearing parameters from 'Smearing'.
C--
      IF ( IRT.EQ.-2 ) THEN
         PRINT *, ' '
         PRINT *, ' >>>>> Caution from SMRRIN >>>>>> '
         PRINT *, '     ''Smearing'' already exists.'
         PRINT *, '     Use the smearing parameters in it. '
C--
         CALL TBGET(2,BKNAM2,2,NW,SMRPRM,JRT)
C--
         ISEED    = IMRPRM( 1)
         BFIELD   = SMRPRM( 2)
C--
         RCYTK(1) = SMRPRM(11)
         RCYTK(2) = SMRPRM(12)
         ZCYTK(1) = SMRPRM(13)
         ZCYTK(2) = SMRPRM(14)
         NSMPTK   = SMRPRM(15)
         NSMPCT   = SMRPRM(16)
         SIGRF    = SMRPRM(17)
         SIGZE    = SMRPRM(18)
C--
         NPHIEM   = SMRPRM(31)
         NTHTEM   = SMRPRM(32)
         NRADEM   = SMRPRM(33)
         RCYEM(1) = SMRPRM(34)
         RCYEM(2) = SMRPRM(35)
         ZCYEM(1) = SMRPRM(36)
         ZCYEM(2) = SMRPRM(37)
         SGEMB0   = SMRPRM(38)
         SGEMB    = SMRPRM(39)
         SGEME0   = SMRPRM(40)
         SGEME    = SMRPRM(41)
C--
         NPHIHD   = SMRPRM(51)
         NTHTHD   = SMRPRM(52)
         NRADHD   = SMRPRM(53)
         RCYHD(1) = SMRPRM(54)
         RCYHD(2) = SMRPRM(55)
         ZCYHD(1) = SMRPRM(56)
         ZCYHD(2) = SMRPRM(57)
         SGHDB0   = SMRPRM(58)
         SGHDB    = SMRPRM(59)
         SGHDE0   = SMRPRM(60)
         SGHDE    = SMRPRM(61)
C--
         NERRVX   = SMRPRM(70)
         NSMPVX   = SMRPRM(71)
         IF ( NSMPVX.GT.MXxVXL ) THEN
            PRINT *, ' >>>>>> Fatal error in SMRPRD >>>>> '
            PRINT *, '   NSMPVX > MXxVXL(too many layers specified)'
            PRINT *, '   terminates this job.'
            PRINT *, '   Increase MXxVXL in /SMRVGO/.'
            STOP
         ENDIF
 
         IF( NERRVX .EQ. 1 .OR. NERRVX.EQ.2 ) THEN
           DPHIVX   = SMRPRM(72)
           DZEEVX   = SMRPRM(73)
           DO 10 LYR = 0, NSMPVX
              IOFF = 4*LYR + 73
              RCYVX(LYR)   = SMRPRM(1+IOFF)
              ZCYVX(1,LYR) = SMRPRM(2+IOFF)
              ZCYVX(2,LYR) = SMRPRM(3+IOFF)
              RDLVX(  LYR) = SMRPRM(4+IOFF)
10         CONTINUE
           IOFF = IOFF + 4
           DO 11 I = 1, 5
             ERRVX(I) = SMRPRM(IOFF+I)
11         CONTINUE

         ELSEIF( NERRVX .EQ. 3 ) THEN
           NUMVTX   = SMRPRM(72)
           ERRVX(1) = SMRPRM(73)
           ERRVX(2) = SMRPRM(74)
           ERRVX(3) = SMRPRM(75)
           ERRVX(4) = SMRPRM(76)
           DO 13 LYR = 0, NSMPVX
              IOFF = 4*LYR + 76
              RCYVX(LYR)   = SMRPRM(1+IOFF)
              ZCYVX(1,LYR) = SMRPRM(2+IOFF)
              ZCYVX(2,LYR) = SMRPRM(3+IOFF)
              RDLVX(  LYR) = SMRPRM(4+IOFF)
13         CONTINUE

         ELSE
            PRINT *,'Fatal Error in SMRRIN ... NERRVX is',NERRVX,
     >      ' which should be 1, 2 or 3.'
            STOP
         ENDIF
C--
C  TBPUT smearing parameters in 'Smearing'.
C--
      ELSE IF ( IRT.GE.0 ) THEN
C--      
         CALL UVZERO(LNxPRM,SMRPRM)
C--
         IMRPRM( 1) = ISEED
         SMRPRM( 2) = BFIELD
C--
         SMRPRM(11) = RCYTK(1)
         SMRPRM(12) = RCYTK(2)
         SMRPRM(13) = ZCYTK(1)
         SMRPRM(14) = ZCYTK(2)
         SMRPRM(15) = NSMPTK
         SMRPRM(16) = NSMPCT
         SMRPRM(17) = SIGRF
         SMRPRM(18) = SIGZE
C--
         SMRPRM(31) = NPHIEM
         SMRPRM(32) = NTHTEM
         SMRPRM(33) = NRADEM
         SMRPRM(34) = RCYEM(1)
         SMRPRM(35) = RCYEM(2)
         SMRPRM(36) = ZCYEM(1)
         SMRPRM(37) = ZCYEM(2)
         SMRPRM(38) = SGEMB0
         SMRPRM(39) = SGEMB
         SMRPRM(40) = SGEME0
         SMRPRM(41) = SGEME
C--
         SMRPRM(51) = NPHIHD
         SMRPRM(52) = NTHTHD
         SMRPRM(53) = NRADHD
         SMRPRM(54) = RCYHD(1)
         SMRPRM(55) = RCYHD(2)
         SMRPRM(56) = ZCYHD(1)
         SMRPRM(57) = ZCYHD(2)
         SMRPRM(58) = SGHDB0
         SMRPRM(59) = SGHDB
         SMRPRM(60) = SGHDE0
         SMRPRM(61) = SGHDE
C--
         SMRPRM(70) = NERRVX
         SMRPRM(71) = NSMPVX

         IF( NERRVX.EQ.1 .OR. NERRVX.EQ.2 ) THEN
           SMRPRM(72) = DPHIVX
           SMRPRM(73) = DZEEVX 
           DO 20 LYR = 0, NSMPVX
             IOFF = 4*LYR + 73
             SMRPRM(1+IOFF) = RCYVX(  LYR)
             SMRPRM(2+IOFF) = ZCYVX(1,LYR)
             SMRPRM(3+IOFF) = ZCYVX(2,LYR)
             SMRPRM(4+IOFF) = RDLVX(  LYR)
20         CONTINUE
           IOFF = IOFF + 4
           DO 21 I = 1, 5
             SMRPRM(I+IOFF) = ERRVX(I)
 21        CONTINUE
           NPARMS = 4*NSMPVX + 77 + 5

         ELSEIF( NERRVX.EQ.3 ) THEN
           SMRPRM(72) = NUMVTX
           SMRPRM(73) = ERRVX(1)
           SMRPRM(74) = ERRVX(2)
           SMRPRM(75) = ERRVX(3)
           SMRPRM(76) = ERRVX(4)
           DO 23 LYR = 0, NSMPVX
             IOFF = 4*LYR + 76
             SMRPRM(1+IOFF) = RCYVX(  LYR)
             SMRPRM(2+IOFF) = ZCYVX(1,LYR)
             SMRPRM(3+IOFF) = ZCYVX(2,LYR)
             SMRPRM(4+IOFF) = RDLVX(  LYR)
23         CONTINUE
           NPARMS = 4*(NSMPVX+1) + 77 
        
         ELSE
           PRINT *,'Fatal error in SMRRIN .. NERRVX is ',NERRVX,
     >             ' which should be 1, 2, or 3'
           STOP
         ENDIF

         CALL TBPUT(2,BKNAM2,2,NPARMS,SMRPRM,JRT)
      ELSE
C--
C  Error.
C--
         PRINT *, ' >>>>> Error in SMRRIN >>>>>> '
         PRINT *, '     Failed to TBCRTE ''Smearing''.'
         PRINT *, '     STOP execution. '
         STOP
      ENDIF

C--
C  Print out input paramters.
C--
      IF ( IRT.NE.-2 ) RETURN
      WRITE(6,*) '  '
      WRITE(6,*) '%--  '
      WRITE(6,*) '%  SMRRIN replaced smearing parameters.' 
      WRITE(6,*) '%--  '
      WRITE(6,*) '  '
      WRITE(6,*) ' Smearing: Parameters for this job.  '
      WRITE(6,*) '  '
      WRITE(6,*) ' General parameters                '
      WRITE(6,*) '        Seed for smearing            ', IMRPRM( 1)
      WRITE(6,*) '        B(kG)                        ', SMRPRM( 2)
      WRITE(6,*) ' Tracking divice parameters        '
      WRITE(6,*) '        R_min                        ', SMRPRM(11)
      WRITE(6,*) '        R_max                        ', SMRPRM(12)
      WRITE(6,*) '        Z_min                        ', SMRPRM(13)
      WRITE(6,*) '        Z_max                        ', SMRPRM(14)
      WRITE(6,*) '        # sampling points            ', SMRPRM(15)
      WRITE(6,*) '        cut on # sampling points     ', SMRPRM(16)
      WRITE(6,*) '        sigma_(r-phi)                ', SMRPRM(17)
      WRITE(6,*) '        sigma_Z                      ', SMRPRM(18)
      WRITE(6,*) ' Electromagnetic calorimeter paramters '
      WRITE(6,*) '        # phi segments               ', SMRPRM(31)
      WRITE(6,*) '        # theta segments (barrel)    ', SMRPRM(32)
      WRITE(6,*) '        # r segments (endcap)        ', SMRPRM(33)
      WRITE(6,*) '        R_min                        ', SMRPRM(34)
      WRITE(6,*) '        R_max                        ', SMRPRM(35)
      WRITE(6,*) '        Z_min                        ', SMRPRM(36)
      WRITE(6,*) '        Z_max                        ', SMRPRM(37)
      WRITE(6,*) '        sigma_E/E pedestal (barrel)  ', SMRPRM(38)
      WRITE(6,*) '        sigma_E/E at 1 GeV (endcap)  ', SMRPRM(39)
      WRITE(6,*) '        sigma_E/E pedestal (barrel)  ', SMRPRM(40)
      WRITE(6,*) '        sigma_E/E at 1 GeV (endcap)  ', SMRPRM(41)
      WRITE(6,*) ' Hadron calorimeter paramters '
      WRITE(6,*) '        # phi segments               ', SMRPRM(51)
      WRITE(6,*) '        # theta segments (barrel)    ', SMRPRM(52)
      WRITE(6,*) '        # r segments (endcap)        ', SMRPRM(53)
      WRITE(6,*) '        R_min                        ', SMRPRM(54)
      WRITE(6,*) '        R_max                        ', SMRPRM(55)
      WRITE(6,*) '        Z_MIN                        ', SMRPRM(56)
      WRITE(6,*) '        Z_max                        ', SMRPRM(57)
      WRITE(6,*) '        sigma_E/E pedestal (barrel)  ', SMRPRM(58)
      WRITE(6,*) '        sigma_E/E at 1 GeV (endcap)  ', SMRPRM(59)
      WRITE(6,*) '        sigma_E/E pedestal (barrel)  ', SMRPRM(60)
      WRITE(6,*) '        sigma_E/E at 1 GeV (endcap)  ', SMRPRM(61)
      WRITE(6,*) ' Vertex detector paramters '
      WRITE(6,*) '        Type of VTX Error assignment ', SMRPRM(70)
      WRITE(6,*) '        # sampling layer + 1         ', SMRPRM(71)

      IOFFV=73
      IF( NERRVX.EQ.1 .OR. NERRVX.EQ.2 ) THEN
        WRITE(6,*) '        phi width                    ', SMRPRM(72)
        WRITE(6,*) '        zee width                    ', SMRPRM(73)
      ELSE
        WRITE(6,*) '        Nuber of VTX layer           ', SMRPRM(72)
        WRITE(6,*) '        phi resol. of VTX            ', SMRPRM(73)
        WRITE(6,*) '        Z resol. of VTX              ', SMRPRM(74)
        WRITE(6,*) '        phi resol. of IT             ', SMRPRM(75)
        WRITE(6,*) '        Z resol. of IT               ', SMRPRM(76)
        IOFFV=76
      ENDIF

      WRITE(6,*)'        R of beam pipe               ',SMRPRM(IOFFV+1)
      WRITE(6,*)'        Z_min of beam pipe           ',SMRPRM(IOFFV+2)
      WRITE(6,*)'        Z_max of beam pipe           ',SMRPRM(IOFFV+3)
      WRITE(6,*)'        thickness of beam pipe in RL ',SMRPRM(IOFFV+4)
      DO 30 LYR = 1, NSMPVX - 1
         IOFF = 4*LYR + IOFFV
         WRITE(6,*) '        R of ', LYR, '-th layer              ',
     .                          SMRPRM(1+IOFF)
         WRITE(6,*) '        Z_min of ', LYR, '-th layer          ',
     .                          SMRPRM(2+IOFF)
         WRITE(6,*) '        Z_max of ', LYR, '-th layer          ',
     .                          SMRPRM(3+IOFF)
         WRITE(6,*) '        thickness of ', LYR, '-th layer(RL)  ',
     .                          SMRPRM(4+IOFF)
30    CONTINUE
      IOFF = 4*NSMPVX + IOFFV
      WRITE(6,*) '        R of CDC inner wall          ',SMRPRM(IOFF+1)
      WRITE(6,*) '        Z_min of CDC inner wall      ',SMRPRM(IOFF+2)
      WRITE(6,*) '        Z_max of CDC inner wall      ',SMRPRM(IOFF+3)
      WRITE(6,*) '        thickness of CDC inner wall  ',SMRPRM(IOFF+4)
      IF( NERRVX .EQ. 2 ) THEN
       IOFF = IOFF + 4
       WRITE(6,*)'        VTX Space point resolution   ',SMRPRM(IOFF+1)
      ENDIF
      WRITE(6,*) '  '
      WRITE(6,*) ' CDC thickness per cm in X0 unit     ',XRAD_CDC

      WRITE(6,*) ' Cal. shower shape parameter         '
      WRITE(6,*) '      EM: a1,  a2    ', CLSSH_ANM(1,1), CLSSH_ANM(2,1)
      WRITE(6,*) '          b1,  b2    ', CLSSH_ALM(1,1), CLSSH_ALM(2,1)
      WRITE(6,*) '      HD: a1,  a2    ', CLSSH_ANM(1,2), CLSSH_ANM(2,2)
      WRITE(6,*) '          b1,  b2    ', CLSSH_ALM(1,2), CLSSH_ALM(2,2)
      WRITE(6,*) '  '
C--
C  That's it.
C--
      RETURN
      END
