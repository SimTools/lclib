C*********************************************************************CC
C*                                                                    *C
C*=======================================--------===                  *C
C* Subroutine UMATIN(MXxDIM,NORDER,ARRAY,DET,IRET)                    *C
C*=======================================--------===                  *C
C*                                                                    *C
C*(Function)                                                          *C
C*    Invert a positive definite real symmetric matrix.               *C
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
C*    Calls DSINV and DSFACT  in CERNLIB (libkernlib.a)
C*(Update Record)                                                     *C
C*   7/01/87  K.Fujii         Original version.                       *C
C*   7/04/87  K.Fujii         Protect for large DET.                  *C
C*   28/3/96  M.Iwasaki       Modify for using CRENLIB                *C
C*                                                                    *C
C*********************************************************************CC
   
      SUBROUTINE UMATIN(MXxDIM,NORDER,ARRAY,DET,IRET)
   
      IMPLICIT    REAL*8 ( A-H, O-Z )
   
      INTEGER*4   MXxDIM, NORDER, IRET
      REAL   *8   ARRAY(MXxDIM,MXxDIM),DET
      REAL   *8   ARRAY1(200,200)
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
      IF ( NORDER.GT.MXxDIM ) THEN
         PRINT *, ' >>>>>> Error in UMATIN >>>>>>'
         PRINT *, '  NORDER > MXxDIM; NORDER,MXxDIM = ', NORDER, MXxDIM
         IRET = - 1
         RETURN
      ENDIF
      do 11 i=1,MXxDIM	
      do 12 j=1,MXxDIM
	ARRAY1(i,j)=ARRAY(i,j)
12    continue
11    continue      
C--
C  Invert Symmetric Positive-Definite Matrix.
C--
      CALL DSINV(NORDER, ARRAY, MXxDIM, ICON)
    
      IF ( ICON.NE.0 ) THEN
         PRINT *, ' >>>>>> Error in UMATIN >>>>>>'
         PRINT *, '  DSINV returns IRET = ',ICON
         IRET = - 2
         RETURN
      ENDIF

      CALL DSFACT(NORDER, ARRAY1, 200, ICON1, DET, ICON2)   

      IF ( ICON2.NE.0 ) THEN
         PRINT *, ' >>>>>> Error in UMATIN >>>>>>'
         PRINT *, ' detA cannot calculated in DSFACT IRET = ',ICON2
         IRET = - 3
         RETURN
      ENDIF
C--
C  That's it.
C--
      RETURN
      END








