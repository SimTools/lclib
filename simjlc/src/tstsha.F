C*    MXxHSG = (2,3) = (EM,HD)
C*
      PARAMETER  ( MXxHSG = 3 )
      REAL   *4  DXY(2), XY(2), EXY(-MXxHSG:MXxHSG,-MXxHSG:MXxHSG)
      REAL   *4  BXY(2,2)
C
C========< Entry Point >================================================
C
C--
C  Set job parameters.
C--
      IPRINT = 1
      NT     = 5
C--
C  Initialize geometry.
C--
      IF ( MXxHSG.EQ.2 ) THEN
         ITYP   = 1
         DXY(1) = 10
         DXY(2) = 10
      ELSE
         ITYP   = 2
         DXY(1) = 20
         DXY(2) = 20
      ENDIF
C--
C  Initialize clocks.
C--
      TIMEA = 0
      TIMEW = 0
      DX = DXY(1)/NT
      DY = DXY(2)/NT
C--
C  Loop over positions.
C--
      DO 2000 IX = 1, NT
      DO 2000 IY = 1, NT
         XY (1) = IX*DX
         XY (2) = IY*DY
         IF ( IPRINT.NE.0 ) THEN
            PRINT *, ' X, Y = ', XY(1), XY(2)
         ENDIF
C--
C  Calculate energy deposit with SMRSHA.
C--
         CALL CLOCK(T1,2,2)
         CALL SMRSHA(ITYP,MXxHSG,DXY,XY,EXY)
         IF ( IPRINT.NE.0 ) THEN
            PRINT *, ' SMRSHA with X,Y=', XY(1), XY(2)
            IF ( MXxHSG.EQ.2 ) THEN
               WRITE(6,'(/5(5(F8.3)/))')
     .         ((EXY(I,J),I=-MXxHSG,MXxHSG),J=-MXxHSG,MXxHSG)
            ELSE
               WRITE(6,'(/7(7(F8.3)/))')
     .         ((EXY(I,J),I=-MXxHSG,MXxHSG),J=-MXxHSG,MXxHSG)
            ENDIF
         ENDIF
         CALL CLOCK(T2,2,2)
         TIMEA = TIMEA + T2 - T1
C--
C  Calculate energy deposit with SMRSHW.
C--
         CALL CLOCK(T1,2,2)
         DO 1000 J = -MXxHSG, MXxHSG
            BXY(1,2) = DXY(2)*J - XY(2)
            BXY(2,2) = BXY(1,2) + DXY(2)
            DO 100 I = -MXxHSG, MXxHSG
               BXY(1,1) = DXY(1)*I - XY(1)
               BXY(2,1) = BXY(1,1) + DXY(1)
               CALL SMRSHW(ITYP,1.,BXY,EPRT)
               EXY(I,J) = EPRT
100         CONTINUE
1000     CONTINUE
         CALL CLOCK(T2,2,2)
         TIMEW = TIMEW + T2 - T1
         IF ( IPRINT.NE.0 ) THEN
            PRINT *, ' SMRSHW with X,Y=', XY(1), XY(2)
            IF ( MXxHSG.EQ.2 ) THEN
               WRITE(6,'(/5(5(F8.3)/))')
     .         ((EXY(I,J),I=-MXxHSG,MXxHSG),J=-MXxHSG,MXxHSG)
            ELSE
               WRITE(6,'(/7(7(F8.3)/))')
     .         ((EXY(I,J),I=-MXxHSG,MXxHSG),J=-MXxHSG,MXxHSG)
            ENDIF
         ENDIF
2000  CONTINUE
C--
C  Print out CPU times.
C--
      TIMEA = TIMEA/NT/NT
      TIMEW = TIMEW/NT/NT
      PRINT *, ' TIMEA = ', TIMEA
      PRINT *, ' TIMEW = ', TIMEW
C--
C  That's it.
C--
      STOP
      END
