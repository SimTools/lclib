      SUBROUTINE ROTXXX(P,Q , PROT)
C
C This subroutine performs the spacial rotation of a four-momentum.
C The momentum P is assumed to be given in the frame where the spacial
C component of Q points the positive z-axis.  PROT is the momentum P
C rotated to the frame where Q is given.
C
C INPUT:
C       real    P(0:3)         : four-momentum P in Q(1)=Q(2)=0 frame
C       real    Q(0:3)         : four-momentum Q in the rotated frame
C
C OUTPUT:
C       real    PROT(0:3)      : four-momentum P in the rotated frame
C
      REAL*8  P(0:3),Q(0:3),PROT(0:3),QT2,QT,PSGN,QQ,P1
C
      PROT(0) = P(0)
C
      QT2=Q(1)**2+Q(2)**2
C
      IF (QT2.EQ.0.D0) THEN
          IF (Q(3).EQ.0.D0) THEN
             PROT(1) = P(1)
             PROT(2) = P(2)
             PROT(3) = P(3)
          ELSE
             PSGN=DSIGN(1.D0,Q(3))
             PROT(1) = P(1)*PSGN
             PROT(2) = P(2)*PSGN
             PROT(3) = P(3)*PSGN
          ENDIF
      ELSE
          QQ=DSQRT(QT2+Q(3)**2)
          QT=DSQRT(QT2)
          P1=P(1)
          PROT(1) = Q(1)*Q(3)/QQ/QT*P1 -Q(2)/QT*P(2) +Q(1)/QQ*P(3)
          PROT(2) = Q(2)*Q(3)/QQ/QT*P1 +Q(1)/QT*P(2) +Q(2)/QQ*P(3)
          PROT(3) =          -QT/QQ*P1               +Q(3)/QQ*P(3)
      ENDIF
      RETURN
      END
