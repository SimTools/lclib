C *********************************************************************C
C*                                                                     *
C*==========================                                           *
C* Subroutine TBLLDT(ID,BNAME,NE)                                      *
C*==========================                                           *
C*(Purpose)                                                            *
C*   List bank_element data                                            *
C*(Input)                                                              *
C*   ID     : DSB ID                                                   *
C*   BNAME  : Bank name                                                *
C*   NE     : Bank_element no.                                         *
C*(Used COMMON)                                                        *
C*  /SSWORK/
C*(Author)                                                             *
C*       A.Miyamoto     June 12 '84                                    *
C*                                                                     *
C **********************************************************************
 
      SUBROUTINE TBLLDT(ID,BNAME,NE)
 
      COMMON /SSWORK/ IDATA(2000)
      CHARACTER*(*) BNAME
C*
C*
C*
      LBNAM = LEN(BNAME)
      WRITE(6,'('' sub. TBLLDT... Bank element dump '')')
      WRITE(6,'(5X,'' DSB ID     ='',I5)') ID
      WRITE(6,'(5X,'' Bank name  ='',A64)') BNAME(1:LBNAM)
      WRITE(6,'(5X,'' Element no.='',I5)') NE
 
      CALL TBGET(ID,BNAME,NE,NWORDS,IDATA,IRET)
 
      DO 100 I=1,NWORDS,5
        I1=I
        I2=I+4
        IF(I2.GT.NWORDS) I2=NWORDS
        WRITE(6,200) (IDATA(K),K=I1,I2)
200     FORMAT(' IDATA=',5I10)
100   CONTINUE
 
      RETURN
      END
