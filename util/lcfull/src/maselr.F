CC*********************************************************************C
C*                                                                     *
C*------------------
C* SUBROUTINE MASELR(COMAND)
C*------------------
C*                                                                     *
C*(Purpose)                                                            *
C*   Set search range of the run record.
C*                                                                     *
C*(Input)                                                              *
C*   ( read from the terminal)                                         *
C*                                                                     *
C*(Output)                                                             *
C*   ( to common /MASLCT/
C*                                                                     *
C*(Author)                                                             *
C*   A. Miyamoto  20-Jan-1987       Original version.                  *
C*                                                                     *
C***********************************************************************
C
      SUBROUTINE MASELR(COMAND)
C
#include "maslct.inc"
#include "marpnt.inc"
#include "macntl.inc"
C
      CHARACTER*(*) COMAND
      CHARACTER     WORK*80, WORK2*80, MESAGE*137
      CHARACTER*8   DTIM1, DTIM2
C
C=======< Entry Point >=================================================
C
C ----------------------------------------------------------------------
C (1) Loop over max record.
C ----------------------------------------------------------------------
C
100   CONTINUE
      WRITE(MCxMPT,110)
110   FORMAT(' Enter END,Vdate,Vtime,Type,Crew,Error,Qual,',/,
     >'      Sdate,Stime,Edate,Etime,RVOL,RFN,FVOL,FFN,FSIZE,FDISK')
      CALL PROMPT(2,' Enter parameter name ?>',IRT)
      READ(MCxMGT,'(A)') WORK(1:20)
      CALL UCSTRP(WORK,' ',WORK2,LWRK)
      CALL UCUPER(LWRK, WORK2)
C
      IF(WORK2(1:3).EQ.'END') THEN
        GO TO 900
      ELSEIF(WORK2(1:5).EQ.'VDATE') THEN
        CALL UCDATE(MSxSLC(1, MPxVDT), DTIM1)
        CALL UCDATE(MSxSLC(2, MPxVDT), DTIM2)
        WRITE(MCxMPT,210) DTIM1, DTIM2
210     FORMAT('  Change Version date range from ',A,' to ',A)
        CALL PROMPT(2,' to ?>',IRT)
        READ(MCxMGT,'(6(I2,1X))') IY1,IM1,ID1,IY2,IM2,ID2
        ITEMP1 = ID1 + 100*IM1 + 10000*IY1
        ITEMP2 = ID2 + 100*IM2 + 10000*IY2
        CALL UCDATE(ITEMP1, DTIM1)
        CALL UCDATE(ITEMP2, DTIM2)
        WRITE(MCxMPT,215) DTIM1, DTIM2
215     FORMAT(' Changed version date range from ',A,' to ',A)
        MSxSLC(1, MPxVDT) = ITEMP1
        MSxSLC(2, MPxVDT) = ITEMP2
 
      ELSEIF(WORK2(1:5).EQ.'VTIME') THEN
        CALL UCTIME(MSxSLC(1, MPxVTM), DTIM1)
        CALL UCTIME(MSxSLC(2, MPxVTM), DTIM2)
        WRITE(MCxMPT,220) DTIM1, DTIM2
220     FORMAT('  Change Version time range from ',A,' to ',A)
        CALL PROMPT(2,' to ?>',IRT)
        READ(MCxMGT,'(6(I2,1X))') IY1,IM1,ID1,IY2,IM2,ID2
        ITEMP1 = 1000*(ID1 + 60*IM1 + 3600*IY1)
        ITEMP2 = 1000*(ID2 + 60*IM2 + 3600*IY2)
        CALL UCTIME(ITEMP1, DTIM1)
        CALL UCTIME(ITEMP2, DTIM2)
        WRITE(MCxMPT,225) DTIM1, DTIM2
225     FORMAT(' Changed version time range from ',A,' to ',A)
        MSxSLC(1, MPxVTM) = ITEMP1
        MSxSLC(2, MPxVTM) = ITEMP2
 
      ELSEIF(WORK2(1:4).EQ.'TYPE') THEN
        WRITE(MCxMPT,230) MSxSLC(1, MPxTYP), MSxSLC(2, MPxTYP)
230     FORMAT('  Change Run type range from ',I8,' to ',I8)
        CALL PROMPT(2,' to ?>',IRT)
        READ(MCxMGT,*) MSxSLC(1, MPxTYP), MSxSLC(2, MPxTYP)
        WRITE(MCxMPT,235) MSxSLC(1, MPxTYP), MSxSLC(2, MPxTYP)
235     FORMAT(' Changed run type range from ',I8,' to ',I8)
 
      ELSEIF(WORK2(1:4).EQ.'CREW') THEN
        WRITE(MCxMPT,240) MSxSLC(1, MPxCLU), MSxSLC(2, MPxCLU)
240     FORMAT('  Change Crew range from ',A ,' to ',A )
        CALL PROMPT(2,' to ?>',IRT)
        READ(MCxMGT,'(A4,1X,A4)') MSxSLC(1, MPxCLU), MSxSLC(2, MPxCLU)
        WRITE(MCxMPT,245) MSxSLC(1, MPxCLU), MSxSLC(2, MPxCLU)
245     FORMAT(' Changed Crew range from ',A ,' to ',A )
 
      ELSEIF(WORK2(1:3).EQ.'ERR') THEN
        WRITE(MCxMPT,250) MSxSLC(1, MPxERR), MSxSLC(2, MPxERR)
250     FORMAT('  Change Error flag range from ',I8,' to ',I8)
        CALL PROMPT(2,' to ?>',IRT)
        READ(MCxMGT,*) MSxSLC(1, MPxERR), MSxSLC(2, MPxERR)
        WRITE(MCxMPT,255) MSxSLC(1, MPxERR), MSxSLC(2, MPxERR)
255     FORMAT(' Changed Error flag range from ',I8,' to ',I8)
 
      ELSEIF(WORK2(1:4).EQ.'QUAL') THEN
        WRITE(MCxMPT,260) MSxSLC(1, MPxQAL), MSxSLC(2, MPxQAL)
260     FORMAT('  Change Run quality range from ',I8,' to ',I8)
        CALL PROMPT(2,' to ?>',IRT)
        READ(MCxMGT,*) MSxSLC(1, MPxQAL), MSxSLC(2, MPxQAL)
        WRITE(MCxMPT,265) MSxSLC(1, MPxQAL), MSxSLC(2, MPxQAL)
265     FORMAT(' Changed Run quality range from ',I8,' to ',I8)
 
      ENDIF
      GO TO 100
C
900   CONTINUE
      RETURN
      END
