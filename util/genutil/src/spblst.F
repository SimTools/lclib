C***********************************************************************
C*
C*  ------------------===
C*  Subroutine SPBLST( LU )
C*  ------------------===
C*(Function)
C*   Print contents of Spring:Parton_List
C*(Input)
C*   LU  ; Output logical unit #
C*
C***********************************************************************
C*
      SUBROUTINE SPBLST( LU )
C*
      COMMON /SSWORK/ NOELM(1000), RBUF(100)
C
C======< Entry Point >==================================================
C
      CALL TBNOEL(1,'Spring:Parton_List',NTRK,NOELM)
      WRITE(LU,200) NTRK
200   FORMAT(1H ,'*** Spring:Parton_List *** ',I4,' tracks',/,
     >' Ser#  PID  Mass  Chg  Px  Py  Pz  E   #Dau 1Dau Par  Hel. Col.')
C
      DO 300 I = 1, NTRK
        CALL TBGET(1,'Spring:Parton_List',NOELM(I),NW,RBUF,IRET)
        WRITE(LU,320) (RBUF(K),K=1,8),(RBUF(K2),K2=12,14),
     >                (RBUF(K3),K3=17,18)
320     FORMAT(F4.0,F5.0,F8.3,F4.0,4F10.4,5F4.0)
300   CONTINUE
      RETURN
      END
