CC********************************************************************CC
C*                                                                    *C
C*===================---==                                            *C
C*  Subroutine UBASV3(E)                                              *C
C*===================---==                                            *C
C*                                                                    *C
C*    1) Purpose                                                      *C
C*        Sets the standard basis vectors.                            *C
C*    2) I/O specification                                            *C
C*        Output                                                      *C
C*          E(*,1) ; x-axis.                                          *C
C*          E(*,2) ; y-axis.                                          *C
C*          E(*,3) ; z-axis.                                          *C
C*    3) Update record                                                *C
C*         6/12/85 TKSF Original version.                             *C
C*                                                                    *C
CC********************************************************************CC
C  
C =====================---=
      SUBROUTINE UBASV3(E)
C =====================---=
C  
      DIMENSION E(3,3)
C  
      DO 10 II=1,3
      DO 10  I=1,3
        E(I,II) = 0.0
 10   CONTINUE
C  
      DO 20  I=1,3
        E(I,I) = 1.0
 20   CONTINUE
C  
      RETURN
      END
