C*********************************************************************CC
C*                                                                    *C
C*=======================================--------===                  *C
C* Subroutine UMATIG(MXxDIM,NORDER,ARRAY,DET,IRET)                    *C
C*=======================================--------===                  *C
C*                                                                    *C
C*(Function)                                                          *C
C*    Invert a real matrix. At present MXxORD is set at 103 which     *C
C*    enables inversion of a matrix with dimension up to 103x103.     *C
C*    User can change MXxORD, if necessary.                           *C
C*    <Caution>                                                       *C
C*    For inversion of positive definite real symmetric matrices,     *C
C*    use UMATIN which is faster and has no restriction to array      *C
C*    size.                                                           *C
C*(Input)                                                             *C
C*    NORDER      : (I*4) ; Order of the matrix. NORDER <= MXxDIM.    *C
C*    MXxDIM      : (I*4) ; ARRAY size. MXxDIM <= MXxORD.             *C
C*    ARRAY(*,*)  : (R*8) ; Input matrix.                             *C
C*(Output)                                                            *C
C*    ARRAY       : (R*8) ; Inverse of the input matrix.              *C
C*    DET         : (R*8) ; Determinant of A.                         *C
C*    IRET        : (I*4) ; return code.                              *C
C*                   = 0  ; OK.                                       *C
C*                   < 0  ; Error.                                    *C
C*(Relation)                                                          *C
C*    Calls DALU, DLUIV in SSLII library.                             *C
C*(Update Record)                                                     *C
C*   7/01/87  K.Fujii         Original version.                       *C
C*                                                                    *C
C*********************************************************************CC
   
      SUBROUTINE UMATIG(MXxDIM,NORDER,ARRAY,DET,IRET)
   
      IMPLICIT    REAL*8 ( A-H, O-Z )
   
      INTEGER*4   MXxDIM, NORDER, IRET
      REAL   *8   ARRAY(MXxDIM,MXxDIM),DET
   
      PARAMETER   ( MXxORD = 103 )
      INTEGER*4   IP(MXxORD)
      REAL   *8   VW(MXxORD)
C  
C========< Entry Point >================================================
C  
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  Check if work area available.
C--
      IF ( NORDER.GT.MXxORD ) THEN
         PRINT *, ' >>>>>> Error in UMATIG >>>>>>'
         PRINT *, '  NORDER > MXxORD; NORDER,MXxORD = ', NORDER, MXxORD
         IRET = - 1
         RETURN
      ENDIF
C--
C  LU-decompose ARRAY.
C--
      CALL DALU(ARRAY,MXxDIM,NORDER,0.D0,IP,IS,VW,ICON)
      IF ( ICON.NE.0 ) THEN
         PRINT *, ' >>>>>> Error in UMATIG >>>>>>'
         PRINT *, '  DALU returns ICON  = ',ICON
         IRET = - 2
         RETURN
      ENDIF
C--
C  Calculate determinant.
C--
      DET = IS
      DO 10 I = 1, NORDER
         DET = DET*ARRAY(I,I)
10    CONTINUE
C--
C  Invert LU-decomposed ARRAY.
C--
      CALL DLUIV(ARRAY,MXxDIM,NORDER,IP,ICON)
      IF ( ICON.NE.0 ) THEN
         PRINT *, ' >>>>>> Error in UMATIG >>>>>>'
         PRINT *, '  DLUIV returns ICON = ',ICON
         IRET = - 3
         RETURN
      ENDIF
C--
C  That's it.
C--
      RETURN
      END
