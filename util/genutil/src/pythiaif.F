C******************************************************************
C*
C*(Description) Interface routines between PYTHIA55 and FULL.
C*
C*(Author) A. Miyamoto   19-Oct-1992   Original version, prepared based.
C*                                     on T#GN.G#LUND73.FORT(JETSETIF)
C*         A. Miyamoto   23-Mar-1995   Modified P55FILL. 
C*                                     IP = MSTI(4) is primary.
C*
C******************************************************************
C*
C
      SUBROUTINE P55EVT ( RECID, LEVEL, DEBUG, QUALIT )
C
C ARGUMENT
C
      INTEGER * 4      RECID
      INTEGER * 4      LEVEL, DEBUG
      INTEGER * 4      QUALIT
C
C COMMON
C
      COMMON   / LUDATR /  MRLU( 6 ), RRLU( 100 )
      COMMON   / LUJETS /  N, K(4000,5), P(4000,5), V(4000,5)
      INTEGER * 4      N, K
      REAL * 4         P
C
C63  COMMON /LUDATE/  MSTE (  40 ), PARE ( 80 )
      COMMON/LUDAT1/   MSTU(200),PARU(200),MSTJ(200),PARJ(200)
      COMMON/RNDMNB/RNJ
      COMMON /LUSEED/  ISEED
      COMMON /LUCNTL/  SAVELU
      LOGICAL * 4      SAVELU
C
CMSP =EXPAND 'T#TP.COM.FORT(COMODE)'
#ifdef UNIX
#include "comode.inc"
#endif 
C
C CONSTANTS
C
      CHARACTER * 40   LUQUES
      DATA   LUQUES    / 'Pythia - Execution Parameters' /
      CHARACTER * 40   LUNAME ( 4 ), FRAME, BEAM, TARGET
      DATA   LUNAME    / 'E_CM','PRINT_FLAG',
     >                   'SEED', 'SAVE_PYHTIA'/
C
C VARIABLES
C
      INTEGER * 4      IFL
      REAL * 4         R
      INTEGER * 4      NFLAV, PLUND
      REAL * 4         ECM
      REAL * 4         VAL ( 4 )
      INTEGER * 4      IVAL ( 4 )
      LOGICAL * 4      LVAL ( 4 )
      EQUIVALENCE      ( VAL, IVAL, LVAL )
      DATA    VAL      / 58.0, 0.0, 0.0, 0.0 /
      INTEGER * 4      NEVENT      / 0 /
      INTEGER * 4      IRET
      INTEGER * 4      CURSED
      INTEGER * 4      INIT /0/
C
C FUNCTIONS
C
      REAL * 4      RLU
C
C MAIN
C
      IF( INIT.EQ.0 ) THEN
        PRINT *,'PYTHIA55: Error in P55EVT..PYTHIA is not initialized.'
        STOP
      ENDIF
      CURSED = ISEED
      CALL PYEVNT

      CALL P55FIL ( NEVENT, ECM, CURSED, IRET )
      IF ( IRET .LT. 0 ) THEN
        QUALIT = -1
      ELSE
        QUALIT = 0
      END IF
      IF ( PLUND .GE. 1 ) THEN
        CALL LULIST ( PLUND )
      ELSE IF( QUALIT .NE. 0 ) THEN
        CALL LULIST ( 0 )
      END IF
      RETURN
C
C----------
C P55COM
C----------
C
      ENTRY P55COM
C
C
C MAIN
C
      FRAME = 'CMS'
      CALL TXTRD('Frame of the exp.:',FRAME )
      BEAM  = 'e-'
      CALL TXTRD('Beam Particle    :',BEAM  )
      TARGET= 'e+'
      CALL TXTRD('Target Particle  :',TARGET)
      CALL CHVAL ( LUQUES, 4, LUNAME, '\HELP', 'RIIL', VAL )
      IF ( ICOMER .NE. 1 ) RETURN
      ECM    = VAL ( 1 )
      PLUND  = IVAL ( 2 )
      ISEED  = IVAL ( 3 )
      SAVELU = LVAL ( 4 )
      IF ( IVAL(3) .NE. 0 ) THEN
        MRLU( 1 ) = ISEED
        MRLU( 2 ) = 0
      ENDIF
C
C
      CALL P55PAR
C
C
C ---------------------
C Call Pythia initialization.
C ---------------------
C
      CALL PYINIT(FRAME, BEAM, TARGET, ECM )
      INIT = 1
C
      RETURN
      END
C
C----------
C P55FIL
C----------
C
      SUBROUTINE P55FIL ( NEVENT, ECM, CURSED, IERR )
C
C ARGUMENTS
C
      INTEGER * 4      NEVENT
      REAL * 4         ECM
      INTEGER * 4      CURSED
      INTEGER * 4      IERR
C
C COMMON
C
      COMMON  / LUJETS / N, K(4000,5), P(4000,5), V(4000,5)
      COMMON / PYPARS / MSTP(200), PARP(200), MSTI(200), PARI(200)

      INTEGER * 4      N, K
      REAL * 4         P
C63  COMMON  / LUDAT3 /   DPAR ( 20 ), IDB ( 120 ), CBR ( 400 ),
C63 #                     KDP (1600 )
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON  / LUCNTL /   SAVELU
      LOGICAL * 4      SAVELU

      COMMON/LUDAT1/   MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C
      COMMON  / SSWORK /   IBUF ( 20000 )
      INTEGER * 4      IBUF
      REAL * 4         RBUF ( 20000 )
      EQUIVALENCE      ( IBUF, RBUF )
C
C CONSTANTS
C
      INTEGER * 4      IDCHG(7000)
      INTEGER * 4      ICHG0( 500),ICHG1( 500)
      INTEGER * 4      ICHG2(1000),ICHG3(1000)
      INTEGER * 4      ICHG4(1000),ICHG5(1000),ICHG6(1000)
      INTEGER * 4      ICHG7( 500),ICHG8( 500)
      EQUIVALENCE (IDCHG(  1),ICHG0(1))
      EQUIVALENCE (IDCHG(501),ICHG1(1))
      EQUIVALENCE (IDCHG(1001),ICHG2(1))
      EQUIVALENCE (IDCHG(2001),ICHG3(1))
      EQUIVALENCE (IDCHG(3001),ICHG4(1))
      EQUIVALENCE (IDCHG(4001),ICHG5(1))
      EQUIVALENCE (IDCHG(5001),ICHG6(1))
      EQUIVALENCE (IDCHG(6001),ICHG7(1))
      EQUIVALENCE (IDCHG(6501),ICHG8(1))
C
      DATA ICHG0 / 202, 201, 203, 204, 205, 206, 0, 0, 0, 0,
     1   3,   2,   5,   4,   7,   6,   0,   0,   0,   0,
     2 300,   1, 101, 102, 103,   0,   0,   0,   0,   0,
C    3   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    4   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    5   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    6   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    7   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    8   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    9   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     3   80*0,
C    #   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     1  12,   0, 112,   0,   0,   0,   0,   0,   0,   0,
     2   0,   0,   0,   0,   0,   0,   0,   0,   0,  18,
C    3   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    4   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    5   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    6   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    7   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    8   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
C    9   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     2   70*0,
     >   0,   0,   0,   0,   0,   0,   0,   0,   0,   0,
     1  11,   0, 111,   0,   0,   0,   0,   0,   0,   0,
     2  13,   0, 113,   0,   0,   0,   0,   0,   0,   0,
     .  70*0,
     +   0,  0,  0,  0,  0,  0,  0, 0, 0, 17,
     1  16,   0, 116,   0,   0,   0,   0,   0,   0,   0,
     2  15,   0, 115,   0,   0,   0,   0,   0,   0,   0,
     3  14,   0, 114,   0,   0,   0,   0,   0,   0,   0,
     .  70*0,
     +  32,   0, 132,   0,   0,   0,   0,   0,   0,   0,
     1  31,   0, 131,   0,   0,   0,   0,   0,   0,   0,
     2  33,   0, 133,   0,   0,   0,   0,   0,   0,   0,
     3  34,   0, 134,   0,   0,   0,   0,   0,   0,   0, 50*0 /
      DATA ICHG1 /
     .  20*0,
     1  42,   0, 142,   0,   0,   0,   0,   0,   0,   0,
     2  41,   0, 141,   0,   0,   0,   0,   0,   0,   0,
     3  43,   0, 143,   0,   0,   0,   0,   0,   0,   0,
     4  44,   0, 144,   0,   0,   0,   0,   0,   0,   0,
     5  45,   0, 145,   0,   0,   0,   0,   0,   0,   0,
     .  50*0,
     1  51,   0, 152,   0,   0,   0,   0,   0,   0,   0,
     2  52,   0, 151,   0,   0,   0,   0,   0,   0,   0,
     3  53,   0, 153,   0,   0,   0,   0,   0,   0,   0,
     4  54,   0, 154,   0,   0,   0,   0,   0,   0,   0,
     5  55,   0, 155,   0,   0,   0,   0,   0,   0,   0,
     6  56,   0, 156,   0,   0,   0,   0,   0,   0,   0,
     . 320*0 /
      DATA ICHG2/ 100*0,
     +   0,   0, 222,   0,   0,   0,   0,   0,   0,   0,
     1   0,   0,   0, 124,   0,   0,   0,   0,   0,   0,
     . 880*0 /
      DATA ICHG3/ 100*0,
     + 221,   0, 212,   0,   0,   0,   0,   0,   0,   0,
     1   0,  22,   0, 123,   0,   0,   0,   0,   0,   0,
     .  80*0,
     +   0,   0, 211,   0,   0,   0,   0,   0,   0,   0,
     1   0,  21,   0, 122,   0,   0,   0,   0,   0,   0,
     2   0,   0,   0, 121,   0,   0,   0,   0,   0,   0,
     . 770*0 /
      DATA ICHG4/ 100*0,
     + 232,   0, 223,   0,   0,   0,   0,   0,   0,   0,
     1   0,  26,   0, 127,   0,   0,   0,   0,   0,   0,
     2   0,  23,   0,   0,   0,   0,   0,   0,   0,   0,
     .  70*0,
     + 231,   0, 213,   0,   0,   0,   0,   0,   0,   0,
     1   0,  25,   0, 126,   0,   0,   0,   0,   0,   0,
     2   0,  24,   0, 125,   0,   0,   0,   0,   0,   0,
     .  70*0,
     +   0,   0, 233,   0,   0,   0,   0,   0,   0,   0,
     1   0,  28,   0, 129,   0,   0,   0,   0,   0,   0,
     2   0,  27,   0, 128,   0,   0,   0,   0,   0,   0,
     3   0,   0,   0, 130,   0,   0,   0,   0,   0,   0,
     . 660*0 /
C
      DATA ICHG5/ 100*0,
     + 242,   0, 224,   0,   0,   0,   0,   0,   0,   0,
     1   0,  66,   0, 163,   0,   0,   0,   0,   0,   0,
     2   0,  61,   0, 163,   0,   0,   0,   0,   0,   0,
     3   0,  63,   0,   0,   0,   0,   0,   0,   0,   0,
     .  60*0,
     + 241,   0,   0, 214,   0,   0,   0,   0,   0,   0,
     1   0,  65,   0, 162,   0,   0,   0,   0,   0,   0,
     2   0,  64,   0, 161,   0,   0,   0,   0,   0,   0,
     3   0,  62,   0,   0,   0,   0,   0,   0,   0,   0,
     .  60*0,
     + 242,   0,   0, 234,   0,   0,   0,   0,   0,   0,
     1   0,  68,   0, 165,   0,   0,   0,   0,   0,   0,
     2   0,  67,   0, 164,   0,   0,   0,   0,   0,   0,
     3   0,  69,   0, 166,   0,   0,   0,   0,   0,   0,
     .  60*0,
     1   0,   0, 244,   0,   0,   0,   0,   0,   0,   0,
     . 590*0 /
C
      DATA ICHG6/ 100*0,
     + 252,   0, 225,   0,   0,   0,   0,   0,   0,   0,
     1   0,  76,   0, 173,   0,   0,   0,   0,   0,   0,
     2   0,  71,   0,   0,   0,   0,   0,   0,   0,   0,
     3   0,  73,   0,   0,   0,   0,   0,   0,   0,   0,
     .  60*0,
     + 251,   0, 215,   0,   0,   0,   0,   0,   0,   0,
     1   0,  75,   0, 172,   0,   0,   0,   0,   0,   0,
     2   0,  74,   0, 171,   0,   0,   0,   0,   0,   0,
     3   0,  72,   0,   0,   0,   0,   0,   0,   0,   0,
     .  60*0,
     + 253,   0, 235,   0,   0,   0,   0,   0,   0,   0,
     1   0,  78,   0, 175,   0,   0,   0,   0,   0,   0,
     2   0,  77,   0, 174,   0,   0,   0,   0,   0,   0,
     3   0,  79,   0, 176,   0,   0,   0,   0,   0,   0,
     .  60*0,
     1 254,   0, 245,   0,   0,   0,   0,   0,   0,   0,
     .  90*0,
     1   0,   0, 255,   0,   0,   0,   0,   0,   0,   0, 490*0 /
C
      DATA ICHG7/  100*0,
     + 262,   0, 226,   0,   0,   0,   0,   0,   0,   0,
     1   0,  86,   0, 183,   0,   0,   0,   0,   0,   0,
     2   0,  81,   0,   0,   0,   0,   0,   0,   0,   0,
     3   0,  83,   0,   0,   0,   0,   0,   0,   0,   0,
     .  60*0,
     + 261,   0, 216,   0,   0,   0,   0,   0,   0,   0,
     1   0,  85,   0, 182,   0,   0,   0,   0,   0,   0,
     2   0,  84,   0, 181,   0,   0,   0,   0,   0,   0,
     3   0,  82,   0,   0,   0,   0,   0,   0,   0,   0,
     .  60*0,
     + 263,   0, 236,   0,   0,   0,   0,   0,   0,   0,
     1   0,  88,   0, 185,   0,   0,   0,   0,   0,   0,
     2   0,  87,   0, 184,   0,   0,   0,   0,   0,   0,
     3   0,  89,   0, 186,   0,   0,   0,   0,   0,   0,
     .  60*0,
     1 264,   0, 246,   0,   0,   0,   0,   0,   0,   0,
     .  90*0 /
      DATA ICHG8 /
     1 265,   0, 256,   0,   0,   0,   0,   0,   0,   0,
     .  90*0,
     1   0,   0, 266,   0,   0,   0,   0,   0,   0,   0,
     . 390*0 /
C
C VARIABLES
C
      INTEGER * 4      I, J
      INTEGER * 4      NELEM
CTTKN  INTEGER * 4      HEADER ( 20 )
      INTEGER * 4      HEADER (100 )
CTAYM      REAL * 4         PTLIST ( 20 )
      INTEGER * 4      NSPAR, NPARTN
      INTEGER * 4      KS, KH, KF
      INTEGER * 4      IDATE, ITIME
      INTEGER * 4      IRET
      CHARACTER * 16   NAME
      INTEGER * 4      NW

      PARAMETER (MAXLST=4000)
      COMMON /SSWORK/ PTLIST(20, MAXLST), MYID(MAXLST), KYID(MAXLST),
     >        NE(MAXLST), NWRDS(MAXLST)

C
C FUNCTIONS
C
C      INTEGER * 4      KLU
C
C MAIN
C
      DO 1000 J = 1, 20
        HEADER ( J ) = 0
C
1000  CONTINUE
C
C     --------------------------
C        Fill 'Particle_List'
C     --------------------------
      NELEM = 0
C
C      PRINT *, '----------------------------'
C      PRINT *,' MSTI(3:7)=',(MSTI(KK),KK=3,7)
C      PRINT *,' MSTU(70)=',MSTU(70),
C     > ' MSTU..=',(MSTU(KK),KK=71,70+MSTU(70))

      DO 2000 J = MSTI(4)+1, N
	KS = K ( J, 1 )
        CALL LUNAME ( K(J,2), NAME )
C A.Miyamoto  Output all particles
C        IF ( KS .EQ. 1 .OR. KS .EQ. 11 ) THEN
C
C .. omit low energy gamma
         IF( P(J,4).LT.0.001 ) THEN
C           PRINT *,' Omit J=',J,' particle.'
           GO TO 2000 
        ENDIF
C
        IF( J.LE.MSTU(73).AND.KS.NE.1 ) THEN 
C          PRINT *,'J=',J,' MSTU(73)=',MSTU(73),' KS=',KS
          GO TO 2000
        ENDIF


         IF( KS.LT.21 ) THEN
          NELEM = NELEM + 1
C          print *,' NELEM =',NELEM

	  MYID( NELEM ) = J
          KYID( J ) = NELEM
	  NE( NELEM ) = NELEM 
	  NWRDS( NELEM ) = 20

	  DO 2010 J1 = 1, 20
	    PTLIST(J1,J) = 0
2010      CONTINUE
	  PTLIST ( 1 ,NELEM) = NELEM
#ifdef TOPAZ
       PTLIST ( 2 ,NELEM) = IDCHG ( ABS(K(J,2)) )
#endif 
#ifdef TOPAZ
       IF ( K(J,2) .LT. 0 ) PTLIST(2,NELEM) = -PTLIST(2,NELEM)
#endif 
#ifdef JLC
          PTLIST(2,NELEM) = K(J,2)
#endif 
	  PTLIST( 3,NELEM ) = P( J, 5 )
          PTLIST( 4,NELEM ) = LUCHGE (K(J,2)) / 3.0
C         PTLIST ( 4 ) = KLU ( J, 3 ) / 3.0
          PTLIST ( 5,NELEM ) = P( J, 1 )
          PTLIST ( 6,NELEM ) = P( J, 2 )
          PTLIST ( 7,NELEM ) = P( J, 3 )
          PTLIST ( 8,NELEM ) = P( J, 4 )

          PTLIST ( 9,NELEM ) = V( J, 1 )*0.1
          PTLIST (10,NELEM ) = V( J, 2 )*0.1
          PTLIST (11,NELEM ) = V( J, 3 )*0.1
	  IF( K(J,4).NE.0 )  PTLIST(12,NELEM) = K(J,5)-K(J,4)+1
C
C .. Set pointer to the mother.

C	  PRINT *,' J=',J,' K(J,3)=',K(J,3)
          IF( K(J,3).LE. MSTI(4) ) THEN
             PTLIST(14,NELEM) = -K(J,3)
          ELSEIF( K(J,3).LE. MSTU(73) ) THEN
             IP = K(J,3)
 2080        CONTINUE
C	     PRINT *,' IP=',IP,' K =',K(IP,3)
             IP = K(IP,3)
             IF( IP.LE. 0 ) THEN
                PRINT *,'Fatal error in P55FIL..IP=',IP
                STOP
             ELSEIF( IP.LE.MSTI(4) ) THEN
                PTLIST(14,NELEM) = -IP
             ELSE
                GO TO 2080
             ENDIF
          ELSE
  	     PTLIST (14,NELEM ) = KYID( K(J,3) ) 
          ENDIF
	  PTLIST (16,NELEM ) = V(J,5)*0.1
          PTLIST (17,NELEM ) = V(J,4)*0.1
C
C .. Darty trick to avoid Pt=0 particle.
C
          PTSQ = P(J,1)**2 + P(J,2)**2
          IF( PTSQ .LT. 1.E-12 ) THEN
             PTLIST(5,NELEM) = 1.E-6
          ENDIF
        ENDIF
2000  CONTINUE
C
C  Re-calculate pointer to the daughter
      DO 2020 J = 1, NELEM
	  IDAU = PTLIST(12, J)
	  IF( IDAU .NE. 0 ) THEN
	    JOR =  MYID( J )
	    JDAU = K(JOR, 4)
	    PTLIST(13,J) = KYID(JDAU)

C       PRINT *,' J, JOR=',J,JOR,' JDAU=',JDAU,' PTLIST=',PTLIST(13,J)

          ENDIF
2020  CONTINUE

      CALL TBMPT(1,'Generator:Particle_List', NELEM, 20, 
     >   NE, NWRDS, PTLIST, IRET )
C
C     ------------------------
C        Fill 'Parton_List'
C     ------------------------
      NPRTN = 0
      DO 2100 I = 1, MSTI(4)
        KS = K(I,1)
        KF = K(I,2)
        KH = K(I,3)
        IF ( ( (KS.EQ.11 .OR. KS.EQ.12) .AND.
     >         ( (ABS(KF).GE.1 .AND. ABS(KF).LE.8) .OR.
     >           (KF .EQ. 21                     ) ) ) .OR.
     >       ( KF .EQ. 22 .AND. KH .EQ. 0 ) ) THEN
          NPRTN = NPRTN + 1
	  NE(NPRTN) = NPRTN
	  NWRDS(NPRTN) = 20

          PTLIST( 1,NPRTN ) = NPRTN
#ifdef TOPAZ
          IF (KF.LE.0) THEN
#endif 
#ifdef TOPAZ
            PTLIST(2,NPRTN) = IDCHG (ABS(KF))
#endif 
#ifdef TOPAZ
            PTLIST(2,NPRTN) = -PTLIST(2)
#endif 
#ifdef TOPAZ
         ELSE
#endif 
#ifdef TOPAZ
            PTLIST(2,NPRTN) = IDCHG(KF)
#endif 
#ifdef TOPAZ
          ENDIF
#endif 
#ifdef JLC
            PTLIST(2,NPRTN) = KF
#endif 
C         PRINT *,' IN LUXJTIF ... KF =', ABS(KF)
C         PRINT *,' IN LUXJTIF ... PTLIST(2) =', PTLIST(2)
C         PTLIST( 2 ) = IDCHG ( KF )
C         IF( K(I,2) .LT. 0 ) PTLIST(2) = -PTLIST(2)
          PTLIST( 3,NPRTN ) = P(I,5)
#ifdef CCC
     PTLIST( 4 ) = FLOAT( KLU( I,3 ) ) / 3.0
#endif 
          PTLIST( 4,NPRTN ) = FLOAT( LUCHGE(K(I,2)) ) / 3.0
          PTLIST( 5,NPRTN ) = P(I,1)
          PTLIST( 6,NPRTN ) = P(I,2)
          PTLIST( 7,NPRTN ) = P(I,3)
          PTLIST( 8,NPRTN ) = P(I,4)
          PTLIST( 9,NPRTN ) = V(I,1)
          PTLIST(10,NPRTN ) = V(I,1)
          PTLIST(11,NPRTN ) = V(I,1)
C         PTLIST( 9 ) = 0.0
C         PRINT *,'Generator:parton_list'
C         PRINT *,'     RDATA( 1) PARTON SERIAL# =',PTLIST(1)
C         PRINT *,'     RDATA( 2) PARTON ID(TOPAZ) =',PTLIST(2)
C         PRINT *,'     RDATA( 3) MASS(GEV) =',PTLIST(3)
C         PRINT *,'     RDATA( 4) CHARGE =',PTLIST(4)
C         PRINT *,'     RDATA( 5) PX =',PTLIST(5)
C         PRINT *,'     RDATA( 6) PY =',PTLIST(6)
C         PRINT *,'     RDATA( 7) PZ =',PTLIST(7)
C         PRINT *,'     RDATA( 8) E =',PTLIST(8)
C         PRINT *,'     RDATA( 9) X =',PTLIST(9)
C         PRINT *,'     RDATA(10) Y =',PTLIST(10)
C         PRINT *,'     RDATA(11) Z =',PTLIST(11)
 
C          CALL TBPUT( 1,'GENERATOR:PARTON_LIST',NPRTN,20,PTLIST,IRET )
        ENDIF
2100  CONTINUE
C
	CALL TBMPT(1,'Generator:Parton_List', NPRTN, 20, 
     >       NE, NWRDS, PTLIST, IRET )
C
C     -------------------
C        Fill 'Header'
C     -------------------
      CALL UIDATE ( IDATE )
#ifdef MSP
      CALL TIME ( ITIME )
#endif 
#ifdef UNIX
      CALL UITIME ( ITIME )
#endif 
      HEADER ( 1 ) = NEVENT
      HEADER ( 2 ) = IDATE
      HEADER ( 3 ) = ITIME
      HEADER ( 4 ) = 10
      HEADER ( 5 ) = CURSED
      HEADER ( 7 ) = ECM / 2.0 * 1000.0
      HEADER ( 8 ) = 0
C+
C Count Up Stable particles
C-
      NSPAR = 0
      DO 10 I = 1, N
        IF ( K(I,1) .EQ. 11 ) THEN
#ifdef CC
    IF ( K(I,2) .LT. 100 ) THEN
#endif 
C63      IF ( IDB ( K(I,2) ) .LE. 0 ) THEN
          IF ( MDCY(LUCOMP(K(I,2)),2) .LE. 0 ) THEN
            NSPAR = NSPAR + 1
          END IF
        END IF
10    CONTINUE
      HEADER ( 6 ) = NSPAR
C+
C Fill Parton_List ( Generator:Header )
C-
      NPARTN = 0
      IERR   = 0
      DO 20 I = 1, N
        KS = K (I,1)
        KF = K (I,2)
        KH = K (I,3)
C       IF ( KS .GE. 15) GOTO 20
C       IF ( KS .LE. 12) GOTO 20
CCC    IF ( KH .EQ. 0 .AND. KF .LE. 600 ) THEN
CCC   IF ( KH .EQ. 0 ) THEN
        IF ( (KS.EQ.11 .OR. KS.EQ.12) .AND.
     >      ((ABS(KF).GE.1 .AND. ABS(KF).LE.8) .OR.
     >        KF.EQ.21)) THEN
          NPARTN = NPARTN + 1
          IF( NPARTN.GT.90 ) THEN
            IF( IERR.EQ.0 ) THEN
              PRINT *,'PYTHIA55 - Too many partons generated.(>90)'
              PRINT *,' rest of them not stored in the bank.'
            ENDIF
            IERR   = -1
          ELSE
            HEADER ( NPARTN + 9 ) = IDCHG ( ABS ( KF ) )
            IF ( KF .LT. 0 ) HEADER(NPARTN+9) = -HEADER(NPARTN+9)
          ENDIF
        END IF
20    CONTINUE
      HEADER ( 9 ) = NPARTN
      IF ( NPARTN .GT.90 ) THEN
        PRINT *, 'PYTHIA55 - Generated ',NPARTN,' partons.'
      ENDIF
      NW  = MIN( NPARTN+9, 99)
      CALL TBPUT( 1, 'Generator:Header', 1, NW, HEADER, IRET )
C+
C Save LUND common if required
C-
      IF ( SAVELU ) THEN
        NW = 1
        IBUF ( 1 ) = N
        DO 3000 I = 1, N
          DO 3001 J = 1, 2
            NW = NW + 1
            IBUF ( NW ) = K ( I, J )
3001      CONTINUE
3000    CONTINUE
        DO 4000 I = 1, N
          DO 4001 J = 1, 5
            NW = NW + 1
            RBUF ( NW ) = P ( I, J )
4001      CONTINUE
4000    CONTINUE
        CALL TBPUT ( 1, 'Generator:Header', 2, NW, IBUF, IRET )
      END IF
      RETURN
      END
C
C----------
C P55PAR
C----------
C
      SUBROUTINE P55PAR
C
C COMMON
C
      COMMON / LUJETS /    N, K(4000,5), P(4000,5), V(4000,5)
      COMMON/LUDAT1/   MSTU(200),PARU(200),MSTJ(200),PARJ(200)
C63   COMMON / LUDAT2 /    KTYP ( 120 ), PMAS ( 120 ), PWID ( 60 ),
C63  #                     KFR ( 80 ), CFR ( 40 )
      COMMON/LUDAT2/KCHG(500,3),PMAS(500,4),PARF(2000),VCKM(4,4)
C63   COMMON / LUDAT3 /    DPAR ( 20 ), IDB ( 120 ), CBR ( 400 ),
C63  #                     KDP (1600 )
      COMMON/LUDAT3/MDCY(500,3),MDME(2000,2),BRAT(2000),KFDP(2000,5)
      COMMON / LUDATE /    MSTE (  40 ), PARE ( 80 )
      COMMON /LUSEED/  ISEED
      COMMON /CFCNTR/  CF, CN, TR
      COMMON /FACT4J/  FACT4J
C
C Commons for PYTHIA
C
      COMMON/PYSUBS/MSEL,MSUB(200),KFIN(2,-40:40),CKIN(200)
      COMMON/PYPARS/MSTP(200), PARP(200),MSTI(200),PARI(200)
C
C VARIABLES
C
      LOGICAL * 4      LOOP
      CHARACTER * 5    NAMARY
      INTEGER * 4      ELEMNT
      CHARACTER * 60   TEXT2
C
C MAIN
C
C ... Initialize PYTHIA common.
C
      MSTP(122) = 1
      MSEL = 0
      DO 1 ISUB = 1, 200
        MSUB(ISUB) = 0
1     CONTINUE
C
      CALL CMxCLR
      PRINT *, 'PYTHIA55 - ',
     > ' Change PYTHIA/LUND73 event generater parameters'
      LOOP = .TRUE.
      NAMARY = ' '
10    CONTINUE
        IF ( .NOT. LOOP ) GOTO 20
        PRINT *,
     >  'ARRAY NAME :  MSTJ, MSTU, PARJ, MSEL, MSUB, KFIN1, KFIN2, ',
     >  'CKIN, MSTP, PARP, MSTI, PARI, PMS1, PMS2, PMS3, PMS4'
        CALL TXTRD ( 'Enter Array Name or EXIT ', NAMARY )
        CALL STRUPC ( 5, NAMARY )
        IF ( NAMARY(1:1) .EQ. 'E' ) THEN
          LOOP = .FALSE.
        ELSE
          IF( NAMARY(1:4) .EQ. 'MSEL' ) THEN
            CALL INTRD( 'Enter MSEL ',MSEL )
          ELSE
            CALL INTRD ( 'Enter Element Number of the array ', ELEMNT )
            IF ( NAMARY(1:4) .EQ. 'MSTJ' ) THEN
              CALL INTRD ( 'Enter MSTJ ', MSTJ(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'MSTU' ) THEN
              CALL INTRD ( 'Enter MSTU ', MSTU(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'PARU' ) THEN
              CALL FLTRD ( 'Enter PARU ', PARU(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'PARJ' ) THEN
              CALL FLTRD ( 'Enter PARJ ', PARJ(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'MSUB' ) THEN
              CALL INTRD ( 'Enter MSUB ', MSUB(ELEMNT) )
            ELSE IF ( NAMARY(1:5) .EQ. 'KFIN1') THEN
              CALL INTRD ( 'Enter KFIN1', KFIN(1,ELEMNT) )
            ELSE IF ( NAMARY(1:5) .EQ. 'KFIN2') THEN
              CALL INTRD ( 'Enter KFIN2', KFIN(2,ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'CKIN' ) THEN
              CALL FLTRD ( 'Enter CKIN ', CKIN(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'MSTP' ) THEN
              CALL INTRD ( 'Enter MSTP ', MSTP(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'MSTI' ) THEN
              CALL INTRD ( 'Enter MSTI ', MSTI(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'PARP' ) THEN
              CALL FLTRD ( 'Enter PARP ', PARP(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'PARI' ) THEN
              CALL FLTRD ( 'Enter PARI ', PARI(ELEMNT) )
            ELSE IF ( NAMARY(1:4) .EQ. 'PMS1' ) THEN
              KCEL = LUCOMP(ELEMNT)
              WRITE(TEXT2,'(''Enter PMAS('',I3,'',1)'')') KCEL
              CALL FLTRD ( TEXT2(:20), PMAS(KCEL,1) )
            ELSE IF ( NAMARY(1:4) .EQ. 'PMS2' ) THEN
              KCEL = LUCOMP(ELEMNT)
              WRITE(TEXT2,'(''Enter PMAS('',I3,'',2)'')') KCEL
              CALL FLTRD ( TEXT2(:20), PMAS(KCEL,2) )
            ELSE IF ( NAMARY(1:4) .EQ. 'PMS3' ) THEN
              KCEL = LUCOMP(ELEMNT)
              WRITE(TEXT2,'(''Enter PMAS('',I3,'',3)'')') KCEL
              CALL FLTRD ( TEXT2(:20), PMAS(KCEL,3) )
            ELSE IF ( NAMARY(1:4) .EQ. 'PMS4' ) THEN
              KCEL = LUCOMP(ELEMNT)
              WRITE(TEXT2,'(''Enter PMAS('',I3,'',4)'')') KCEL
              CALL FLTRD ( TEXT2(:20), PMAS(KCEL,4) )
            ELSE
              PRINT *, '%PYTHIA55-F, No such array name!'
            END IF
          END IF
        END IF
      GOTO 10
20    CONTINUE
 
 
      RETURN
      END
C
C----------
C P55BRU
C----------
C
      SUBROUTINE P55BRU ( LEVEL, DEBUG )
C
C ARGUMENTS
C
      INTEGER * 4      LEVEL, DEBUG
C
C NOTE
C
C ---------------------------------------------- C
C     Fill 'Data_Type'.                          C
C       IDATA(1)   = (1,2) <--> (Real,MC)        C
C            (2)   = (0,1,2,3) <--> (G,ON,OFF,C) C
C            (3)   = 1 for generator data        C
C            (4-9) = 0 for generator data        C
C ---------------------------------------------- C
C
C ---------------------------------------------- C
C     Fill 'Generator'.                          C
C       IDATA(1)   = DATE ( YYMMDD )             C
C            (2)   = TIME in msec from 0:00      C
C            (3-6) = program name                C
C            (7)   = version number              C
C ---------------------------------------------- C
C
C COMMON ( BLOCK DATA )
C
      EXTERNAL         LUDATA
      EXTERNAL         PYDATA
CEX   XTERNAL         LUEDAT
CEX   EXTERNAL         LUHDAT
C
C VARIABLES
C
      CHARACTER*16 PGNAME, GNNAME
      DATA PGNAME / 'PYTHIA 5.5' /, IVERS / 2 /
C
      DIMENSION    IDATA(9),         JDATA(8),ZDATA(8)
      EQUIVALENCE (IDATA(3),GNNAME),(JDATA(1),ZDATA(1))
C
      INTEGER * 4      IENV
C
C MAIN
C
      CALL FULENV ( IENV )
      IF ( IENV .NE. 1 ) RETURN
C
      PRINT *, PGNAME,' - Creating Begin_Run Record.'
C
C     CALL TBINIT(2,IRET)
      CALL TBCRTE(2,'Data_Type',0,0,IRET)
C        Modified so as not to over-write the bank Data_Type
C        if it exists.
C
       IF( IRET .LE. 0 ) THEN
         CALL TBGET(2,'Data_Type',1,NW,IDATA,IRET)
       ELSE
         DO 10 I = 3,9
           IDATA(I) = 0
10       CONTINUE
       ENDIF
       IDATA(1) = 2
       IDATA(2) = 0
       IDATA(3) = IDATA(3) + 1
      CALL TBPUT(2,'Data_Type', 1,9,IDATA,IRET)
C
      CALL TBCRTE(2,'Generator',0,0,IRET)
      CALL UIDATE(IDATE)
#ifdef MSP
      CALL TIME  (ITIME)
#endif 
#ifdef UNIX
       CALL UITIME  (ITIME)
#endif 
      IDATA (1) = IDATE
      IDATA (2) = ITIME
      GNNAME    = PGNAME
      IDATA (7) = IVERS
      CALL TBPUT(2,'Generator',1, 7,IDATA,IRET)
C
      RETURN
      END
C
C----------
C P55ERU
C----------
C
      SUBROUTINE P55ERU ( LEVEL, DEBUG )
C
C ARGUMENTS
C
      INTEGER * 4      LEVEL, DEBUG
C
C COMMON
C
      COMMON /LUSEED/  ISEED
      COMMON /LUDATR/  MRLU( 6 ), RRLU( 100 )
#ifdef MSP
CMSP =EXPAND 'T#TP.COM.FORT(COMODE)'
#endif 
#ifdef UNIX
#include "comode.inc"
#endif 
C
C VARIABLES
C
      INTEGER * 4      IDATE, ITIME
      INTEGER * 4      IDATA ( 8 )
C
C MAIN
C
CCCC  IF ( BATCH ) CALL LUEEVT ( 10, 0.0 )
C
      CALL PYSTAT(1)
C
      CALL TBCRTE ( 8, 'Generator', 0, 0, IRET )
      CALL UIDATE ( IDATE )
#ifdef MSP
      CALL TIME   ( ITIME )
#endif 
#ifdef UNIX
      CALL UITIME   ( ITIME )
#endif 
      IDATA (1) = IDATE
      IDATA (2) = ITIME
      IDATA (3) = MRLU(1)
      IDATA (4) = MRLU(2)
      IDATA (5) = MRLU(3)
      IDATA (6) = MRLU(4)
      IDATA (7) = MRLU(5)
      IDATA (8) = MRLU(6)
      CALL TBPUT ( 8, 'Generator', 1, 8, IDATA, IRET)
      RETURN
      END
C
C----------
C P55INI
C----------
C
      SUBROUTINE P55INI
C
C COMMON
C
      COMMON /LUSEED/  ISEED
      COMMON /LUDATR/  MRLU( 6 ), RRLU( 100 )
C
C VARIABLES
C
      LOGICAL * 4      ISTAT
#ifdef MSP
      CHARACTER * 4    UID
#endif 
      CHARACTER*44     DSN
C
C MAIN
C
      IF( ISEED .NE. 0 ) THEN
#ifdef MSP
        CALL GETUID ( UID )
#endif 
#ifdef MSP
        DSN = UID//'.@.PYTHIA.SEED'
#endif 
#ifdef UNIX
       DSN= 'PYTHIA.SEED'
#endif 
        INQUIRE ( FILE=DSN, EXIST=ISTAT )
        IF ( ISTAT ) THEN
#ifdef MSP
         OPEN ( UNIT=1, FILE=DSN, FORM='UNFORMATTED',
#endif 
#ifdef MSP
     >         STATUS='OLD', ACTION='READ', ERR=99 )
#endif 
#ifdef UNIX
          OPEN ( UNIT=1, FILE=DSN, FORM='UNFORMATTED',
#endif 
#ifdef UNIX
     >         STATUS='OLD', ERR=99 )
#endif 
          CALL RLUSET( 1, 0 )
          CLOSE ( UNIT = 1 )
          PRINT *,'%PYTHIA-I, Seed was read from file'
        ENDIF
      ENDIF
98    CONTINUE
      PRINT *,' Initial seeds for JETSET73 : MRLU(1) = ',MRLU(1)
      PRINT *,'                            : MRLU(2) = ',MRLU(2)
      PRINT *,'                            : MRLU(3) = ',MRLU(3)
      PRINT *,'                            : MRLU(4) = ',MRLU(4)
      PRINT *,'                            : MRLU(5) = ',MRLU(5)
      PRINT *,'                            : MRLU(6) = ',MRLU(6)
      RETURN
99    CONTINUE
      PRINT *,'%PYTHIA  Error in P55INI.. Error to read SEEDs from',
     >        ' a file ',DSN
      GO TO 98
      END
C
C----------
C P55TRM
C----------
C
      SUBROUTINE P55TRM
C
C COMMON
C
      COMMON /LUSEED/  ISEED
      COMMON /LUADTR/  MRLU(6), RRLU(100)
C
C VARIABLES
C
C      LOGICAL * 4      ISTAT
#ifdef MSP
      CHARACTER * 4    UID
#endif 
      INTEGER * 4      ISEED
      CHARACTER*40     DSN
C
C MAIN
C
#ifdef MSP
      CALL GETUID ( UID )
#endif 
#ifdef MSP
      DSN = UID//'.@.PYTHIA.SEED'
#endif 
#ifdef MSP
      OPEN ( UNIT=1, FILE=DSN, FORM='UNFORMATTED',
#endif 
#ifdef MSP
     >       STATUS='UNKNOWN', ACTION='WRITE', ERR=99 )
#endif 
#ifdef UNIX
       DSN= 'PYTHIA.SEED'
#endif 
#ifdef UNIX
       OPEN ( UNIT=1, FILE=DSN, FORM='UNFORMATTED',
#endif 
#ifdef UNIX
     >       ERR=99 )
#endif 
      CALL RLUGET( 1, 0 )
      CLOSE ( UNIT = 1 )
      PRINT *,'%PYTHIA55, Seed was saved in file:',DSN
      PRINT *,'           Seed was              : MRLU( 1 ) = ', MRLU(1)
      PRINT *,'                                 : MRLU( 2 ) = ', MRLU(2)
      PRINT *,'                                 : MRLU( 3 ) = ', MRLU(3)
      PRINT *,'                                 : MRLU( 4 ) = ', MRLU(4)
      PRINT *,'                                 : MRLU( 5 ) = ', MRLU(5)
      PRINT *,'                                 : MRLU( 6 ) = ', MRLU(6)
99    CONTINUE
      RETURN
      END
