C***********************************************************************
C*
C*===================
C* Subroutine SMRPRD
C*===================
C*
C* (Function)
C*    Reads parameters for smearing.
C* (Update Record)
C*    04/21/90  K.Fujii          Original version.
C*    05/25/90  K.Fujii          Input stream for tracking device
C*                               parameters has been changed.
C*    02/04/91  K.Fujii          Read in vertex detector geometry.
C*    03/31/93  K.Fujii          Fixed a bug in CLSPAR initialization.
C*    09/01/95  K.Fujii          ISEED now saved as an integer.
C*
C***********************************************************************
 
      SUBROUTINE SMRPRD
 
#include "smrvgo.inc"
#include "smrtgo.inc"
#include "smrego.inc"
#include "smrhgo.inc"
#include "smrres.inc"
#include "smrrnd.inc"
#include "smrfld.inc"
#include "smrclp.inc"
#include "prfeld.inc"
      CHARACTER*40    INPUT
      PARAMETER      ( LNxPRM = 200 )
      INTEGER*4       IMRPRM(LNxPRM)
      REAL   *4       SMRPRM(LNxPRM)
      EQUIVALENCE    (IMRPRM,SMRPRM)
C
C========< Entry Point >================================================
C
C--
C  Set default paramters.
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
      SMRPRM(72) = DPHIVX
      SMRPRM(73) = DZEEVX
      DO 10 LYR = 0, NSMPVX
         IOFF = 4*LYR + 73
         SMRPRM(1+IOFF) = RCYVX(  LYR)
         SMRPRM(2+IOFF) = ZCYVX(1,LYR)
         SMRPRM(3+IOFF) = ZCYVX(2,LYR)
         SMRPRM(4+IOFF) = RDLVX(  LYR)
10    CONTINUE
      IOFF = IOFF+4
      DO 11 I=1, 5
         SMRPRM(I+IOFF)= ERRVX(I)
 11   CONTINUE
C--
C  Read in input paramters.
C--
      NRET = 0
      NMOD = 0
100   IADR = 0
      CALL INTRD('SMRPRD Address ? ', IADR )
      IF( IADR .GT.1000 ) THEN
        IP = MOD(IADR-1001,5) + 1
        JP = (IADR-1001)/5 + 1
        CALL FLTRD(' Value ? ', CLSPAR(IP,JP) )
        NMOD = NMOD + 1
        GO TO 100
      ENDIF
 
      IF( IADR .EQ. 0 .OR. IADR .GT. LNxPRM )    GO TO 200
C--
      IF ( IADR.EQ.1 ) THEN
         CALL INTRD(' Value ? ', IMRPRM(IADR) )
      ELSE IF ( IADR.EQ.2 ) THEN
         CALL FLTRD(' Value ? ', SMRPRM(IADR) )
         IMODBF(2) = 1
         BFDATA(2) = SMRPRM(IADR)
      ELSE
         CALL FLTRD(' Value ? ', SMRPRM(IADR) )
      ENDIF
      NMOD = NMOD + 1
                                                 GO TO 100
200   INPUT = '?'
      CALL TITRD('EXIT ?:', INPUT )
C--
C  Update paramters.
C--
      ISEED    = IMRPRM( 1)
      IF ( SMRPRM(2).LT.1.E-2 ) THEN
         PRINT *, '     BFIELD = ', SMRPRM(2), ' is too small.'
         PRINT *, '     Will use BFIELD = 0.01 kG.'
         SMRPRM(2) = 1.E-2
      ENDIF
      PTOR     = PTOR*BFIELD/SMRPRM(2)
      ROPT     = 1/PTOR
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
      IF ( NSMPVX.GT.MXxVXL ) THEN
         PRINT *, ' >>>>>> Fatal error in SMRPRD >>>>> '
         PRINT *, '   NSMPVX > MXxVXL( too many vtx layers specified )'
         PRINT *, '   terminates this job.'
         PRINT *, '   Increase MXxVXL in /SMRVGO/.'
         STOP
      ENDIF
      NERRVX   = SMRPRM(70)
      NSMPVX   = SMRPRM(71)
      DPHIVX   = SMRPRM(72)
      DZEEVX   = SMRPRM(73)
      DO 30 LYR = 0, NSMPVX
         IOFF = 4*LYR + 73
         RCYVX(LYR)   = SMRPRM(1+IOFF)
         ZCYVX(1,LYR) = SMRPRM(2+IOFF)
         ZCYVX(2,LYR) = SMRPRM(3+IOFF)
         RDLVX(  LYR) = SMRPRM(4+IOFF)
30    CONTINUE
      IOFF = 4*(LYR+1)+73
      DO 31 I = 1, 5
         ERRVX(I) = SMRPRM(IOFF+1)
 31   CONTINUE

C--
C  Print out input paramters.
C--
      WRITE(6,*) '%SMRPRD modified ',NMOD,' parameters.'
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
      WRITE(6,*) '        Type of Error assignment     ', SMRPRM(70)
      WRITE(6,*) '        # sampling layer + 1         ', SMRPRM(71)
      WRITE(6,*) '        phi width                    ', SMRPRM(72)
      WRITE(6,*) '        zee width                    ', SMRPRM(73)
      WRITE(6,*) '        R of beam pipe               ', SMRPRM(74)
      WRITE(6,*) '        Z_min of beam pipe           ', SMRPRM(75)
      WRITE(6,*) '        Z_max of beam pipe           ', SMRPRM(76)
      WRITE(6,*) '        thickness of beam pipe in RL ', SMRPRM(77)
      DO 40 LYR = 1, NSMPVX - 1
         IOFF = 4*LYR + 77
         WRITE(6,*) '        R of ', LYR, '-th layer              ',
     .                          SMRPRM(1+IOFF)
         WRITE(6,*) '        Z_min of ', LYR, '-th layer          ',
     .                          SMRPRM(2+IOFF)
         WRITE(6,*) '        Z_max of ', LYR, '-th layer          ',
     .                          SMRPRM(3+IOFF)
         WRITE(6,*) '        thickness of ', LYR, '-th layer(RL)  ',
     .                          SMRPRM(4+IOFF)
40    CONTINUE
      IOFF = 4*NSMPVX + 73
      WRITE(6,*) '        R of CDC inner wall          ', SMRPRM(IOFF+1)
      WRITE(6,*) '        Z_min of CDC inner wall      ', SMRPRM(IOFF+2)
      WRITE(6,*) '        Z_max of CDC inner wall      ', SMRPRM(IOFF+3)
      WRITE(6,*) '        thickness of CDC inner wall  ', SMRPRM(IOFF+4)
      IOFF = IOFF + 4
      WRITE(6,*) '        VTX rphi/z error             ', SMRPRM(IOFF+1)
C--
C  That's it.
C--
      RETURN
      END
