C***********************************************************************
C*
C*================================----===
C* Subroutine SMRTBG(LEVEL,IDEBUG,IRET)
C*================================----===
C*
C* (Purpose)
C*     Prepare generator data in /PRJUNK/ by TBGETting
C*     Generator:Particle_List and Generator:Header.
C* (Inputs)
C*        LEVEL    : (I*4) :  level flag.
C*        IDEBUG   : (I*4) :  debug flag.
C* (Output)
C*        IRET     : (I*4) :  return flag.
C* (Relation)
C*     Called by SMREVT.
C*     Calls the following routines;
C*        TBGET.
C* (Updaterecord)
C*     4/18/90  K.Fujii           Original version.
C*
C***********************************************************************
 
      SUBROUTINE SMRTBG(LEVEL,IDEBUG,IRET)
 
#include "smrgdt.inc"
      CHARACTER*64 BKNAM1  / 'Generator:Header' /
      CHARACTER*64 BKNAM2  / 'Generator:Particle_List' /
      DATA NPRINT / 0 /
C
C========< Entry Point >================================================
C
C--
C  Reset return flag.
C--
      IRET = 0
C--
C  TBGET 'Generator:Header'.
C--
      CALL TBGET(1,BKNAM1,1,NW,IGHEAD,IRT)
      IF ( IRT.LT.0 ) THEN
         IRET = IRT
         PRINT *, ' SMRTBG failed to TBGET ', BKNAM1
         PRINT *, '   IRT = ', IRT
         RETURN
      ENDIF
C--
      IGEVNT = IGHEAD(1)
      EBEAM  = 1.E-3*IGHEAD(7)
      NPRTN  = IGHEAD(9)
      NPRTCL = IGHEAD(6)
C--
C  Check # elements in 'Generator:Particle_List'.
C--
      CALL TBNOEL(1,BKNAM2,NE,IEPLST)
      IF ( NPRTCL.NE.NE ) THEN
         IF ( NPRINT.LE.10 ) THEN
            NPRINT = NPRINT + 1
C           PRINT *, ' >>>>>> Error in SMRTBG >>>>>>>'
C           PRINT *, '    NE set in Generator:Header is incosistent '
C           PRINT *, '    with actual NE in Generator:Particle_List.'
C           PRINT *, '      NE(Header) = ', NPRTCL
C           PRINT *, '      NE(P_list) = ', NE
C           PRINT *, '    Will use actual NE.'
         ENDIF
         NPRTCL = NE
      ELSE IF ( IEPLST(NE).GT.MXxPT ) THEN
         PRINT *, '>>>>>> Error in SMRTBG >>>>>>'
         PRINT *, '   MXxPT < IELMX = ', IEPLST(NE)
         PRINT *, '   STOP.'
         STOP
      ENDIF
C--
C  TBGET 'Generator:Particle_List'.
C--
      CALL UVZERO(LNxPT*MXxPT,RPLIST)
      CALL UVZERO(LNxHLX*MXxPT,RHELIX)
      DO 1000 IE = 1, NE
         IPT = IEPLST(IE)
         IF ( IPT.NE.IE .AND. NPRINT.LE.10 ) THEN
            NPRINT = NPRINT + 1
            PRINT *, ' >>>>>> Error in SMRTBG >>>>>>>'
            PRINT *, '    El# is not serial in ',BKNAM2
         ENDIF
         CALL TBGET(1,BKNAM2,IPT,NW,RPLIST(1,IPT),IRT)
         IF ( IRT.LT.0 ) THEN
            IRET = IRT
            PRINT *, ' SMRTBG failed to TBGET el#', IPT, ' of ', BKNAM2
            PRINT *, '   IRT = ', IRT
            RETURN
         ELSE IF ( NW.NE.LNxPT ) THEN
            PRINT *, ' SMRTBG TBGETs wrong #words = ', NW
            PRINT *, ' from el#', IPT, ' of ', BKNAM2
            PRINT *, ' STOP.'
            STOP
         ENDIF
C--
C  Convert (X,Y,Z) and (Px,Py,Pz) to helix parameters.
C  Even a neutral particle is parametrized as a helix with
C  an infinite radius.
C--
         ICHG = RPLIST( 4,IPT)
         CALL UP2HLX(RPLIST(9,IPT),RPLIST(5,IPT),ICHG,RHELIX(1,IPT))
1000  CONTINUE
C--
C  That's it.
C--
      RETURN
      END
