CC**********************************************************************
C*
C*===================================================--------------===
C*  Subrouitne UFCLST(I0IN,J0IN,IMN,IMX,JMN,JMX,IHST,IRNG,JRNG,IOUT)
C*===================================================--------------===
C*
C* (Purpose)
C*     Find 2-dim. clusters.
C*     Currently alowed cluster size is MXxJ in J-direction and
C*     MXxBD/2 for # J-clusters in a single I.
C* (Inputs)
C*     I0IN    :(I*4): starting bin # for I-axis.
C*     J0IN    :(I*4): starting bin # for J-axis.
C*     IHST(I,J) ; 2-dim. histogram, I=IMN..IMX, J=JMN..JMX ( I*2 ).
C* (Outputs)
C*     IRNG(k) ; I range ( k = 1/2 = lower/upper )
C*     JRNG(k) ; J range ( k = 1/2 = lower/upper )
C*     IOUT(*) ; cluster data ( I*2 ).
C*
C*             +==================================================+
C*      D   1  ! # rows for this cluster.                         !
C*      i      +==================================================+
C*      r   2  ! row # ( J ).                                     !
C*      e   3  ! pointer to row # J ( k1 ).                       !
C*      c      +--------------------------------------------------+
C*      t   4  ! row # ( J + 1 ).                                 !
C*      o   5  ! pointer to row # J + 1 ( k2 ).                   !
C*      r      +--------------------------------------------------+
C*      y      !   ......                                         !
C*             +==================================================+
C*          k1 ! # clusters in row # J.                           !
C*      C      +--------------------------------------------------+
C*      l   +1 ! start column # -+-- of 1-st cluster in this row. !
C*      u   +2 ! length         -+                                !
C*      s      +--------------------------------------------------+
C*      t   +3 ! start column # -+-- of 2-nd cluster in this row. !
C*      e   +4 ! length         -+                                !
C*      r      +--------------------------------------------------+
C*             !   ......                                         !
C*
C* (Relation)
C*     Invoked by
C*     Calls
C* (Update record)
C*     9/17/86  K. Fujii  Original version.
C*     9/18/86  K. Fujii  Step vectors are now in DATA, which have
C*                        been calculated step by step before.
C*     7/11/92  K. Fujii  This version treats wrap-around problem
C*                        for J-axis. Arguments are now all I*4.
C*
CC**********************************************************************
 
      SUBROUTINE UFCLST(I0IN,J0IN,IMN,IMX,JMN,JMX,IHST,IRNG,JRNG,IOUT)
 
      DIMENSION IHST(IMN:IMX,JMN:JMX),IOUT(*)
      INTEGER*4 I0IN, J0IN, IMN, IMX, JMN, JMX, IRNG(2), JRNG(2)
      INTEGER*4 IHST, IOUT
      PARAMETER ( MXxJ = 155, MXxBD = 20 )
      INTEGER*4 NBD(-MXxJ+1:MXxJ), IBD(MXxBD,-MXxJ+1:MXxJ), IP(MXxBD)
      INTEGER*4 KD(2,3,0:3)
      DATA      KD / -1, 0, -1, 0, -1,-1,   0, 1,  0, 0, -1, 0,
     .                1, 0,  0,-1,  0, 0,   0,-1, -1,-1,  0,-1  /
C
C==================< Entry Point >======================================
C
C--
C  Set the starting step.
C--
      J0   = J0IN
      J0MN = JMX
      DO 10 I0 = I0IN, IMN, -1
         II0  = MAX(I0-1,IMN)
         IF ( IHST(II0,J0).LE.0 .OR. I0.EQ.IMN ) THEN
            IX      = I0
            JX      = J0
            ISTP    =  1
            ISTPS   =  ISTP
                                                 GO TO 11
         ENDIF
10    CONTINUE
C--
C  Initialize boundary data.
C--
11    NBN  = 0
      CALL UVZERO(2*MXxJ,NBD)
      JOFF = JMX - JMN + 1
C--
C  Decide the next step.
C--
1     IXR  = IX + KD(1,2,ISTP)
      JXR  = JX + KD(2,2,ISTP)
      IF ( JXR.GT.JMX ) THEN
         JXRC = JXR - JOFF
      ELSE IF ( JXR.LT.JMN ) THEN
         JXRC = JXR + JOFF
      ELSE
         JXRC = JXR
      ENDIF
      IF ( ( IXR.LT.IMN ) .OR. ( IXR.GT.IMX ) )  GO TO 3
      IXL  = IX + KD(1,3,ISTP)
      JXL  = JX + KD(2,3,ISTP)
      IF ( JXL.GT.JMX ) THEN
         JXLC = JXL - JOFF
      ELSE IF ( JXL.LT.JMN ) THEN
         JXLC = JXL + JOFF
      ELSE
         JXLC = JXL
      ENDIF
      IF ( ( IXL.GE.IMN ) .AND. ( IXL.LE.IMX ) ) THEN
         IF ( IHST(IXL,JXLC).GT.0 )              GO TO 3
      ENDIF
      IF ( IHST(IXR,JXRC).LE.0 )                 GO TO 3
                                                 GO TO 5
3     IF ( ISTP-ISTPS.LT.1 ) THEN
         ISTP = MOD(ISTP+1,4)
                                                 GO TO 1
      ELSE
         ISTP = MOD(ISTP+2,4)
         IXR  = IX + KD(1,2,ISTP)
         JXR  = JX + KD(2,2,ISTP)
      ENDIF
C--
C  Step accepted. If JX changed, record this step.
C--
5     IF ( KD(2,1,ISTP).NE.0 ) THEN
         JBN      = JXR - J0
         IF ( NBD(JBN).EQ.0 ) NBN = NBN + 1
         NBD(JBN) = NBD(JBN) + 1
         IBD(NBD(JBN),JBN) = IXR
      ENDIF
C--
C  Check if we are back at the start position.
C--
      IX = IX + KD(1,1,ISTP)
      JX = JX + KD(2,1,ISTP)
      IF ( JX.LT.J0MN ) J0MN = JX
      IF ( IX.NE.I0 .OR. JX.NE.J0 ) THEN
         ISTPS = ISTP
                                                 GO TO 1
      ENDIF
C--
C  A cluster found. Fill the output data.
C--
      JRNG(1) = J0MN
      JRNG(2) = J0MN + NBN - 1
      IRNG(1) = IMX
      IRNG(2) = IMN
      IOUT(1) = NBN
      IPNT    = 2*NBN + 1
50    DO 600 KBN = 0, NBN - 1
         JBN = KBN + J0MN - J0
         N = NBD(JBN)
         IF ( N.GT.2 ) THEN
            CALL USORTI(N,1,1,IBD(1,JBN),IP)
         ELSE
            IF ( IBD(1,JBN).LE.IBD(2,JBN) ) THEN
               IP(1) = 1
               IP(2) = 2
            ELSE
               IP(1) = 2
               IP(2) = 1
            ENDIF
         ENDIF
         MNI = IBD(IP(1),JBN)
         MXI = IBD(IP(N),JBN)
         IF ( MNI.LT.IRNG(1) ) IRNG(1) = MNI
         IF ( MXI.GT.IRNG(2) ) IRNG(2) = MXI
         LPNT = 2*(KBN+1)
         IOUT(LPNT  ) = JBN + J0
         IPNT         = IPNT + 1
         IOUT(LPNT+1) = IPNT
         IOUT(IPNT  ) = N/2
         DO 60 I = 1, N, 2
            IFST       = IBD(IP(I),JBN)
            ILST       = IBD(IP(I+1),JBN)
            IPNT       = IPNT + 1
            IOUT(IPNT) = IFST
            IPNT       = IPNT + 1
            IOUT(IPNT) = ILST - IFST + 1
60       CONTINUE
600   CONTINUE
C--
C  That's it.
C--
      RETURN
      END
