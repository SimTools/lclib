CC********************************************************************CC
C*                                                                    *C
C*====================                                                *C
C* Subroutine UERTRM                                                  *C
C*====================                                                *C
C*  1) Purpose                                                        *C
C*       Prints out error statistics.                                 *C
C*  2) Relation                                                       *C
C*       Called by;                                                   *C
C*       Calls;                                                       *C
C*           None.                                                    *C
C*  4) Update record                                                  *C
C*       09/13/85  K.Fujii  Temporary version.                        *C
C*       05/27/86  K.Fujii  Fixed scaler ID.                          *C
C*       05/01/87  K.Fujii  Added JSKLR.                              *C
C*       05/06/87  K.Fujii  Removed JSKLR. ISKLR is 2-dim. instead.   *C
C*                                                                    *C
CC********************************************************************CC
   
      SUBROUTINE UERTRM
#include "uskalr.inc"
      CHARACTER*5 DTNAME(14)
      DATA DTNAME
     .    / 'STER ','IDC  ','TPC  ','BTOF ','BDC  ','BCL  ',
     .      'BMU  ','ETOF ','EDC  ','ECL  ','LUM  ','HYB  ',
     .      'XXX  ','XXX  ' /
   
C--
C  Print out error statistics.
C--
      WRITE(6,'(/2X,''<< Error Statistics for This Job >>         '')')
      WRITE(6,'(/2X,'' (1) ID < 1000                              '')')
      WRITE(6,'(/3X,''  Part                       Error Frequency'')')
      WRITE(6,615)
      LINES = MXxSKL/10 + 1
      IFST  = 0
      ILST  = 9
      DO 100 I = 1, LINES
         ILST = MIN(MXxSKL,ILST)
         WRITE(6,610) DTNAME(I),(ISKLR(ID,1),ID=IFST,ILST)
610      FORMAT(5X,1A5,10I6)
         IFST = IFST + 10
         ILST = ILST + 10
100   CONTINUE
      WRITE(6,615)
615   FORMAT( 3X,'-------+-----+-----+-----+-----+-----+-----+-----+'
     .          ,'-----+-----+-------')
   
      WRITE(6,'(/2X,'' (2) ID >  999                              '')')
      WRITE(6,'(/3X,''  Part                       Error Frequency'')')
      WRITE(6,615)
      LINES = MXxSKL/10 + 1
      IFST  = 0
      ILST  = 9
      DO 200 I = 1, LINES
         ILST = MIN(MXxSKL,ILST)
         WRITE(6,610) DTNAME(I),(ISKLR(ID,2),ID=IFST,ILST)
         IFST = IFST + 10
         ILST = ILST + 10
200   CONTINUE
      WRITE(6,615)
   
      RETURN
      END
