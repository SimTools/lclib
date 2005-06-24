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
C*    03/29/99  A.Miyamoto       Modify parameter read of VTX 
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
      IF( NERRVX .EQ. 1 .OR. NERRVX .EQ.2 ) THEN
        SMRPRM(71) = NSMPVX
        SMRPRM(72) = DPHIVX
        SMRPRM(73) = DZEEVX
        DO 10 LYR = 0, NSMPVX
           IOFF = 4*LYR + 73
           SMRPRM(1+IOFF) = RCYVX(  LYR)
           SMRPRM(2+IOFF) = ZCYVX(1,LYR)
           SMRPRM(3+IOFF) = ZCYVX(2,LYR)
           SMRPRM(4+IOFF) = RDLVX(  LYR)
10      CONTINUE
        IOFF = IOFF+4
        DO 11 I=1, 5
           SMRPRM(I+IOFF)= ERRVX(I)
   11   CONTINUE

      ELSEIF( NERRVX .EQ. 3 ) THEN
        SMRPRM(71) = NSMPVX
        SMRPRM(72) = NUMVTX
        SMRPRM(73) = ERRVX(1)
        SMRPRM(74) = ERRVX(2)
        SMRPRM(75) = ERRVX(3)
        SMRPRM(76) = ERRVX(4)
        DO 13 LYR = 0, NSMPVX
           IOFF = 4*LYR + 76
           SMRPRM(1+IOFF) = RCYVX(  LYR)
           SMRPRM(2+IOFF) = ZCYVX(1,LYR)
           SMRPRM(3+IOFF) = ZCYVX(2,LYR)
           SMRPRM(4+IOFF) = RDLVX(  LYR)
13      CONTINUE
        
      ELSE
         PRINT *,'Error in SMRPRD .. VTX parameter type NERRVX=',NERRVX,
     >    ' which must be 1 , 2 or 3'
         STOP
      ENDIF

C--
C  Read in input paramters.
C--
      NRET = 0
      NMOD = 0
100   IADR = 0
      CALL INTRD('SMRPRD Address ? ', IADR )
      IF( IADR .GT.1000 .AND. IADR .LT. 1100 ) THEN
        IP = MOD(IADR-1001,5) + 1
        JP = (IADR-1001)/5 + 1
        CALL FLTRD(' Value ? ', CLSPAR(IP,JP) )
        NMOD = NMOD + 1
        GO TO 100
      ENDIF

      IF( IADR .GT.1100 .AND. IADR .LT. 1200 ) THEN
        IP = MOD(IADR-1100,100)
        JP = IP/10 + 1
        IP = MOD(IP,10)
        IF(IP.EQ.1.OR.IP.EQ.2) THEN
           CALL FLTRD(' Value ? ', CLSSH_ANM(IP,JP))
           NMOD = NMOD + 1
        ELSE IF(IP.EQ.3.OR.IP.EQ.4 ) THEN
           CALL FLTRD(' Value ? ', CLSSH_ALM(IP-2,JP))
           NMOD = NMOD + 1
        ENDIF
        GO TO 100
      ENDIF
C      DATA (CLSSH_ANM(I,1),I=1,2)/ 0.055424, 0.395883/
C      DATA (CLSSH_ALM(I,1),I=1,2)/ 0.45,  1.20 /
C      DATA (CLSSH_ANM(I,2),I=1,2)/ 0.0877193, 0.043860/
C      DATA (CLSSH_ALM(I,2),I=1,2)/ 2.20,  7.00/
      
      IF( IADR .EQ. 2001 ) THEN
        CALL FLTRD(' Value ? ',XRAD_CDC ) 
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
      NERRVX   = SMRPRM(70)
      NSMPVX   = SMRPRM(71)
      IF ( NSMPVX.GT.MXxVXL ) THEN
         PRINT *, ' >>>>>> Fatal error in SMRPRD >>>>> '
         PRINT *, '   NSMPVX > MXxVXL( too many vtx layers specified )'
         PRINT *, '   terminates this job.'
         PRINT *, '   Increase MXxVXL in /SMRVGO/.'
         STOP
      ENDIF

      IF( NERRVX .EQ. 1 .OR. NERRVX .EQ. 2 ) THEN
        DPHIVX   = SMRPRM(72)
        DZEEVX   = SMRPRM(73)
        DO 310 LYR = 0, NSMPVX
          IOFF = 4*LYR + 73
          RCYVX(LYR)   = SMRPRM(1+IOFF)
          ZCYVX(1,LYR) = SMRPRM(2+IOFF)
          ZCYVX(2,LYR) = SMRPRM(3+IOFF)
          RDLVX(  LYR) = SMRPRM(4+IOFF)
310      CONTINUE
        IOFF = 4*(LYR+1)+73
        DO 312 I = 1, 5
          ERRVX(I) = SMRPRM(IOFF+1)
 312    CONTINUE
      
      ELSEIF( NERRVX .EQ. 3 ) THEN
        NUMVTX   = SMRPRM(72)
        ERRVX(1) = SMRPRM(73)
        ERRVX(2) = SMRPRM(74)
        ERRVX(3) = SMRPRM(75)
        ERRVX(4) = SMRPRM(76)
        DO 330 LYR = 0, NSMPVX
          IOFF = 4*LYR + 76
          RCYVX(LYR)   = SMRPRM(1+IOFF)
          ZCYVX(1,LYR) = SMRPRM(2+IOFF)
          ZCYVX(2,LYR) = SMRPRM(3+IOFF)
          RDLVX(  LYR) = SMRPRM(4+IOFF)
330      CONTINUE

      ELSE
        PRINT *,'Fatal error in SMRPRD .. Parameter data format',
     >  ', NERRVX is ',NERRVX,' which must be 1, 2 or 3'
        STOP
      ENDIF

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
      
      IOFFV=73
      IF( NERRVX .EQ. 1 .OR. NERRVX .EQ. 2 ) THEN
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
        
      WRITE(6,*) '        R of beam pipe               ',SMRPRM(IOFFV+1)
      WRITE(6,*) '        Z_min of beam pipe           ',SMRPRM(IOFFV+2)
      WRITE(6,*) '        Z_max of beam pipe           ',SMRPRM(IOFFV+3)
      WRITE(6,*) '        thickness of beam pipe in RL ',SMRPRM(IOFFV+4)
      DO 40 LYR = 1, NSMPVX - 1
         IOFF = 4*LYR + IOFFV
         WRITE(6,*) '        R of ', LYR, '-th layer              ',
     .                          SMRPRM(1+IOFF)
         WRITE(6,*) '        Z_min of ', LYR, '-th layer          ',
     .                          SMRPRM(2+IOFF)
         WRITE(6,*) '        Z_max of ', LYR, '-th layer          ',
     .                          SMRPRM(3+IOFF)
         WRITE(6,*) '        thickness of ', LYR, '-th layer(RL)  ',
     .                          SMRPRM(4+IOFF)
40    CONTINUE
      IOFF = 4*NSMPVX + IOFFV
      WRITE(6,*) '        R of CDC inner wall          ',SMRPRM(IOFF+1)
      WRITE(6,*) '        Z_min of CDC inner wall      ',SMRPRM(IOFF+2)
      WRITE(6,*) '        Z_max of CDC inner wall      ',SMRPRM(IOFF+3)
      WRITE(6,*) '        thickness of CDC inner wall  ',SMRPRM(IOFF+4)
      IF( NERRVX .EQ. 2 ) THEN
       IOFF = IOFF + 4
       WRITE(6,*)'        VTX rphi/z error             ',SMRPRM(IOFF+1)
      ENDIF

      WRITE(6,*) ' CDC thickness per cm in X0 unit     ',XRAD_CDC

      WRITE(6,*) ' Cal. shower shape parameter         '
      WRITE(6,*) '      EM: a1,  a2    ', CLSSH_ANM(1,1), CLSSH_ANM(2,1)
      WRITE(6,*) '          b1,  b2    ', CLSSH_ALM(1,1), CLSSH_ALM(2,1)
      WRITE(6,*) '      HD: a1,  a2    ', CLSSH_ANM(1,2), CLSSH_ANM(2,2)
      WRITE(6,*) '          b1,  b2    ', CLSSH_ALM(1,2), CLSSH_ALM(2,2)

C--   
C  That's it.
C--
      RETURN
      END

