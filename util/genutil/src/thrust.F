C   17/01/87 711032221  MEMBER NAME  THRUST   (FORT)     M  FORTRAN             
C-----------------------------------------------------------------------        
      SUBROUTINE SPHER ( NUMPRT, PRTLST, LNGDAT, IPX0,                          
     .                  SPH   , APL   , EIGVCT        )                         
C-----------------------------------------------------------------------        
                                                                                
C...FUNCTION: CALCULATE SPHERICITY AND APLANALITY                               
                                                                                
C...INPUTS:                                                                     
      INTEGER     NUMPRT, LNGDAT, IPX0                                          
      REAL*4      PRTLST(LNGDAT,1), RVAL                                        
      CHARACTER*3 COMAND                                                        
                                                                                
C...OUTPUTS:                                                                    
      REAL*4      SPH, APL, EIGVCT(0:3,3)                                       
                                                                                
C...ERROR FLAGS:                                                                
C     SPH = APL = -1                                                            
                                                                                
C...MODIFIED VER. OF LUSPHER IN THE LUND MONTE CARLO V.4.3.                     
C     ALL INPUTS AND OUTPUTS ARE TRANSFERED BY SUBROUTINES CALLS.               
C     MODIFIED BY HIRO YAMAMOTO ON FEB. 26, 1983                                
                                                                                
C...ARGUMENTS: SUBROUTINE SPHER                                                 
C     NUMPRT : # PARTICLES FOR ANALYSIS FILLED IN PRTLST. ( :== NP )            
C     PRTLST : MOMENTUM LIST OF PARTICLES.                                      
C              PRTLST(IPX0+J,I) = PX, PY, PZ OF I'TH PATICLES FOR J = 1 , 3     
C              ( PRTLST(IPX0+J,I) :== P(J,I') FOR J = 1 , 3 WHEN I AND I'       
C                CORESPONDS TO THE SAME PARTICLE. )                             
C     LNGDAT : DATA LENGTH OF ONE PARTICLE IN PRTLST                            
C     IPX0   : BASE FOR THE THREE MOMENTUM IN PRTLST.                           
C              PX = PRTLST(IPX0+1), PY = PRTLST(IPX0+2), PZ = PRTLST(IPX0+3)    
C     SPH    : SPHERICITY = 3(EIGVCT(0,2)+EIGVCT(0,3))/2                        
C     APL    : APLANALITY = 3 EIGVCT(0,3) / 2                                   
C     EIGVCT : EIGEN VECTOR OF SPHERICITY TENSOR.                               
C              EIGVCT(0,J) : J'TH EIGEN VALUE                                   
C              EIGVCT(I,J) : J'TH EIGEN VECTOR                                  
C                 SPHERICITY TENSOR(I,J) = SUM[ P(A,I)*P(A,J)*P(A)**(R-2) ]     
C                                        / SUM[ P(A)**R ]                       
C                   WHERE P(A,I) IS THREE MOMENTUM OF PARTICLE A,               
C                   P(A) IS MOMENTUM OF A AND R IS A PARAMETER(:==PARE(30)).    
C                 EIGVCT(0,1) > EIGVCT(0,2) > EIGVCT(0,3)                       
                                                                                
C...ARGUMENTS: ENTRY SPHPRM                                                     
C     COMAND : CONTROL TO GET OR PUT PARAMETER VALUE.                           
C              COMAND = 'GET' RETURNS CURRENT PARAMETER VALUE.                  
C              COMAND = 'PUT' RESETS PARAMETER VALUE.                           
C     RVAL   : NEW/OLD VALUE OF R                                               
                                                                                
C...PARAMETERS:                                                                 
      REAL*8  R / 2. /                                                          
C             R:==PARE(30)                                                      
                                                                                
C...LOCAL VARIABLES:                                                            
      REAL*8  SM(3,3), SV(3,3), DEV(0:3,3)                                      
      REAL*8  ZERO / 0.0 /                                                      
      REAL*8  PSQSUM, PWT, SMAX, SQ, SR, SP, RL, NORM                           
                                                                                
C.......................................................................        
C  BODY:                                                                        
C...VERY LOW MULTIPLICITIES ( 0 OR 1 ) NOT ACCEPTED                             
      IF ( NUMPRT .LE. 1 ) THEN                                                 
        SPH = -1                                                                
        APL = -1                                                                
        RETURN                                                                  
      END IF                                                                    
                                                                                
C...CALCULATE MAXTRIX TO BE DIAGONALIZED.                                       
      DO 100 L1 =  1 , 3                                                        
      DO 100 L2 = L1 , 3                                                        
          SM(L1,L2) = 0                                                         
  100 CONTINUE                                                                  
      PSQSUM = 0                                                                
      DO 120 I  =  1 , NUMPRT                                                   
        NORM = SQRT( PRTLST(IPX0+1,I)**2 + PRTLST(IPX0+2,I)**2 +                
     .               PRTLST(IPX0+3,I)**2                         )              
        IF ( ABS(R-2) .GT. 0.001 ) THEN                                         
          PWT = NORM**(R-2)                                                     
        ELSE                                                                    
          PWT = 1                                                               
        END IF                                                                  
        DO 110 L1 =  1 , 3                                                      
        DO 110 L2 = L1 , 3                                                      
          SM(L1,L2) = SM(L1,L2)                                                 
     .              + PWT*PRTLST(IPX0+L1,I)*PRTLST(IPX0+L2,I)                   
  110   CONTINUE                                                                
        PSQSUM = PSQSUM + PWT*NORM**2                                           
  120 CONTINUE                                                                  
                                                                                
      IF ( PSQSUM .LE. 0 ) THEN                                                 
C...    PROTECTION AGAINST NULL MOMENTUM                                        
        SPH = -1                                                                
        APL = -1                                                                
        RETURN                                                                  
      END IF                                                                    
                                                                                
      DO 130 L1 =  1 , 3                                                        
      DO 130 L2 = L1 , 3                                                        
        SM(L1,L2) = SM(L1,L2) / PSQSUM                                          
  130 CONTINUE                                                                  
                                                                                
C...FIND EIGENVALUES TO MATRIX ( THIRD DEGREE EQUATION )                        
      SQ = (   SM(1,1)*SM(2,2) + SM(1,1)*SM(3,3) + SM(2,2)*SM(3,3)              
     .       - SM(1,2)**2      - SM(1,3)**2      - SM(2,3)**2     ) / 3.        
      SR = -0.5 * ( SQ +                                                        
     .              SM(1,1)*SM(2,3)**2 + SM(2,2)*SM(1,3)**2 +                   
     .              SM(3,3)*SM(1,2)**2 - SM(1,1)*SM(2,2)*SM(3,3) )              
     .   + SM(1,2)*SM(1,3)*SM(2,3) + 1./27.                                     
      SQ = 1./9. - SQ                                                           
      SQ = SQRT(MAX(SQ,ZERO))                                                   
      SP = COS( ACOS(SR/SQ**3) / 3 )                                            
      DEV(0,1) = 1./3. + 2*SP*SQ                                                
      DEV(0,3) = MAX ( 1./3. - SQ*(SQRT(3*(1-SP**2))+SP), ZERO )                
      DEV(0,2) = MAX ( 1. - DEV(0,1) - DEV(0,3), ZERO )                         
                                                                                
C...FIND FIRST AND LAST EIGEN VECTOR BY SOLVING EQUATION SYSTEM                 
      DO 170 LD = 1 , 3 , 2                                                     
        DO 140 L1 = 1 , 3                                                       
          SV(L1,L1) = SM(L1,L1) - DEV(0,LD)                                     
          DO 141 L2 = L1+1 , 3                                                  
            SV(L1,L2) = SM(L1,L2)                                               
            SV(L2,L1) = SM(L1,L2)                                               
  141     CONTINUE                                                              
  140   CONTINUE                                                                
        SMAX = 0                                                                
        DO 150 L1 = 1 , 3                                                       
        DO 150 L2 = 1 , 3                                                       
          IF ( ABS(SV(L1,L2)) .GT. SMAX ) THEN                                  
            LI = L1                                                             
            LJ = L2                                                             
            SMAX = ABS(SV(L1,L2))                                               
          END IF                                                                
  150   CONTINUE                                                                
        SMAX = 0                                                                
        DO 160 L3 = LI+1 , LI+2                                                 
C...      LI = 1 --> L1 = 2 OR 3 ....                                           
          L1 = L3 - 3*((L3-1)/3)                                                
          IF( SV(LI,LJ) .EQ. 0.0 ) GOTO 900                                     
          RL = SV(L1,LJ) / SV(LI,LJ)                                            
          DO 161 L2 = 1 , 3                                                     
            SV(L1,L2) = SV(L1,L2) - RL*SV(LI,L2)                                
            IF ( ABS(SV(L1,L2)) .GT. SMAX ) THEN                                
              LK = L1                                                           
              SMAX = ABS(SV(L1,L2))                                             
            END IF                                                              
  161     CONTINUE                                                              
  160   CONTINUE                                                                
C...    LJ = 1 --> LJ1 = 2, LJ2 = 3                                             
        LJ1 = LJ + 1 - 3*(LJ/3)                                                 
        LJ2 = LJ + 2 - 3*((LJ+1)/3)                                             
        DEV(LJ1,LD) = -SV(LK,LJ2)                                               
        DEV(LJ2,LD) =  SV(LK,LJ1)                                               
        IF( SV(LI,LJ) .EQ. 0.0 ) GOTO 900                                       
        DEV(LJ ,LD) =-(SV(LI,LJ1)*DEV(LJ1,LD) + SV(LI,LJ2)*DEV(LJ2,LD))         
     .              /  SV(LI,LJ)                                                
        NORM = SQRT ( DEV(1,LD)**2 + DEV(2,LD)**2 + DEV(3,LD)**2 )              
        IF( NORM .LE. 0.0 ) GOTO 900                                            
        DO 171 J = 1 , 3                                                        
          DEV(J,LD) = DEV(J,LD) / NORM                                          
  171   CONTINUE                                                                
  170 CONTINUE                                                                  
                                                                                
C...MIDDLE EIGENVECTOR ORTHOGONAL TO OTHER TWO.                                 
      DEV(1,2) = DEV(2,3)*DEV(3,1) - DEV(3,3)*DEV(2,1)                          
      DEV(2,2) = DEV(3,3)*DEV(1,1) - DEV(1,3)*DEV(3,1)                          
      DEV(3,2) = DEV(1,3)*DEV(2,1) - DEV(2,3)*DEV(1,1)                          
                                                                                
      SPH = 1.5 * ( DEV(0,2) + DEV(0,3) )                                       
      APL = 1.5 *   DEV(0,3)                                                    
                                                                                
      DO 200 I = 1 , 3                                                          
      DO 200 J = 0 , 3                                                          
        EIGVCT(J,I) = DEV(J,I)                                                  
  200 CONTINUE                                                                  
                                                                                
      RETURN                                                                    
C                                                                               
900   SPH = -1                                                                  
      APL = -1                                                                  
      RETURN                                                                    
                                                                                
C-----------------------------------------------------------------------        
      ENTRY SPHPRM(COMAND,RVAL)                                                 
C-----------------------------------------------------------------------        
                                                                                
      IF ( COMAND .EQ. 'GET' ) THEN                                             
        RVAL = R                                                                
      ELSE IF ( COMAND .EQ. 'PUT' ) THEN                                        
        R = RVAL                                                                
      END IF                                                                    
      RETURN                                                                    
                                                                                
      END                                                                       
C                                                                               
C***********************************************************************        
C-----------------------------------------------------------------------        
      SUBROUTINE THRUST( NUMPRT, PRTLST, LNGDAT, IPX0,                          
     .                   THR   , OBL   , EIGVCT        )                        
C-----------------------------------------------------------------------        
                                                                                
C...FUNCTION: CALCURATE THRUST AND OBLATENESS.                                  
                                                                                
C...INPUTS:                                                                     
      INTEGER     NUMPRT, LNGDAT, IPX0                                          
      REAL        PRTLST(LNGDAT,1), RVAL                                        
      CHARACTER*3 COMAND                                                        
      CHARACTER*6 PARAM                                                         
                                                                                
C...OUTPUTS:                                                                    
      REAL        THR, OBL, EIGVCT(0:3,3)                                       
                                                                                
C...ERROR FLAGS:                                                                
C     THR = OBL = -1                                                            
                                                                                
C...MODIFIED VER. OF LUTHRU IN THE LUND MONTE CARLO V.4.3.                      
C      ALL INPUTS AND OUTPUTS ARE TRANSFERED BY SUBROUTINES CALLS.              
C      IN THE ORIGINAL PROGRAMS, IT HAPPENS THAT THE MAJOR BECOMES              
C      SMALLER THAN THE MINOR WHEN ACCURACY OR ITERATIONS ARE NOT               
C      ENOUGH.  IN THIS VERSION, THE MAXIMUM SEARCH IS CONTINUED                
C      UNTIL MAJOR BECOMES LARGER THAN MINOR.  IF THE ORDER IS                  
C      STILL REVERSED WHEN ALL INITIAL DIRECTIONS ARE USED TO SEARCH,           
C      MAJOR AND MINOR ARE EXCHANGED.                                           
C      MODIFIED BY HIRO YAMAMOTO ON FEB. 23, 1983.                              
                                                                                
C...ARGUMENTS OF SUBROUTINE THRUST:                                             
C     NUMPRT : # PARTICLES FOR ANALYSIS FILLED IN PRTLST. ( :== NP )            
C     PRTLST : MOMENTUM LIST OF PARTICLES.                                      
C              PRTLST(IPX0+J,I) = PX, PY, PZ OF I'TH PATICLES FOR J = 1 , 3     
C              ( PRTLST(IPX0+J,I) :== P(J,I') FOR J = 1 , 3 WHEN I AND I'       
C                CORESPONDS TO THE SAME PARTICLE. )                             
C     LNGDAT : DATA LENGTH OF ONE PARTICLE IN PRTLST                            
C     IPX0   : BASE FOR THE THREE MOMENTUM IN PRTLST.                           
C              PX = PRTLST(IPX0+1), PY = PRTLST(IPX0+2), PZ = PRTLST(IPX0+3)    
C     THR    : THRUST                                                           
C     OBL    : OBLATENESS                                                       
C     EIGVCT : EIGEN VECTOR                                                     
C              EIGVCT(I,J) = THRUST AXIS FOR J = 1.                             
C                            MAJOR  AXIS FOR J = 2.                             
C                            MINOR  AXIS FOR J = 3.                             
C                            THRUST VALUE IN THAT DIRECTION FOR I = 0.          
C                            NORMALIZED DIRECTION VECTOR FOR I = 1 , 3.         
C              EIGVCT(0,J) :== P(N+J,4), EIGVCT(I,J) :== P(N+J,I) FOR J = 1 , 3 
                                                                                
C...ARGUMENTS OF ENTRY THRPRM:                                                  
C     COMAND : CONTROL TO GET OR PUT PARAMETER VALUE.                           
C              COMAND = 'GET' RETURNS CURRENT PARAMETER VALUE.                  
C              COMAND = 'PUT' RESETS PARAMETER VALUE.                           
C     PARAM  : IDENTIFY PARAMETER TO CHANGE.                                    
C              PARAM = 'MSTE15', 'PARE31', 'PARE34'                             
C     RVAL   : VALUE OF MSTE(15), PARE(31), PARE(34)                            
C              BE CAREFULL THAT ALL PARAMETERS ARE PASSED BY REAL*4 TYPE.       
                                                                                
C...PARAMETERS:                                                                 
      INTEGER MAXPRT                                                            
      PARAMETER ( MAXPRT = 200 )                                                
      INTEGER M1 / 4 /, M2 / 2 /                                                
      REAL    R  / 1 /, ACCUR / 0.0001 /                                        
C         MSTE(15) = M1*10 + M2                                                 
C         PARE(31) = R                                                          
C         PARE(34) = ACCUR                                                      
C            M1 = LARGE MOMENTUM PARTICLES USED TO SET INITIAL DIRECTION        
C            M2 = NUMBER OF TRIAL INITIAL DIRECTION                             
C            R  = POWER OF MOMENTUM DEPENDENCE.  THRUST FOR R = 1 IS THE        
C                 STANDARD THRUST.                                              
C            ACCUR = CONVERGENCE CRITERION.                                     
                                                                                
C...LOCAL VARIABLES:                                                            
      REAL    PWT(MAXPRT), PLARGE(5,11), DIRINI(4,11), PSUM(3)                  
      REAL    THRDIR(3), MATRIX(3,3), ZAXIS(3) / 0., 0., 1. /                   
      REAL    OLDTHR, NEWTHR                                                    
                                                                                
C...MEANING OF LOCAL VARIABLES:                                                 
C     PLARGE : MOMENTUM OF M1 HIGHEST P PARTICLES                               
C     NC     : # THRUST CANDIDATES CONSTRUCTED BY PARTICLES IN PLARGE.          
C     INIDIR : # OF TRIAL INITAL DIRECTIONS = MIN( NC, 10 )                     
C     DIRINI : TRIAL INITIAL DIRECTIONS                                         
                                                                                
C.......................................................................        
C...BODY:  PROGRAM STARTS HERE.                                                 
                                                                                
C...  VERY LOW MULTIPLICITIES ( 0 OR 1 ) NOT CONSIDERED.                        
      IF ( NUMPRT .LE. 1 .OR. NUMPRT .GT. MAXPRT ) THEN                         
        IF ( NUMPRT .GT. MAXPRT )                                               
     .    WRITE(6,*) '%ERROR IN THRUST:  TOO MANY PARTICLES IN PRTLST.',        
     .               ' MAX = ', MAXPRT                                          
        THR = -1                                                                
        OBL = -1                                                                
        EIGVCT(0,1) = -1                                                        
        EIGVCT(0,2) = -1                                                        
        EIGVCT(0,3) = -1                                                        
        RETURN                                                                  
      END IF                                                                    
                                                                                
C...GO:                                                                         
      DO 10 LD = 1 , 3                                                          
        EIGVCT(0,LD) = 0                                                        
   10 CONTINUE                                                                  
      PS     = 0                                                                
      NC     = 2**( MIN(M1,NUMPRT) - 1 )                                        
      INIDIR = MIN ( 10, NC )                                                   
                                                                                
C...THRUST AND MAJOR AXIS SEARCH.  LD = 1 <==> THRUST, LD = 2 <==> MAJOR        
      DO 280 LD = 1 , 2                                                         
        IF ( LD .EQ. 2 ) THEN                                                   
C...      ROTATE THRUST AXIS ALONG Z DIRECTION FOR MAJOR AXIS SEARCH.           
          CALL ROTMTX( EIGVCT(1,1), ZAXIS, MATRIX )                             
          CALL VCTROT( NUMPRT, PRTLST, LNGDAT, IPX0, MATRIX, 1 )                
        END IF                                                                  
                                                                                
C...    PICKUP M1 LARGE MOMENTUM PARTICLES IN DECREASING ORDER.                 
        DO 100 LF = 1 , M1+1                                                    
          PLARGE(4,LF) = 0                                                      
  100   CONTINUE                                                                
        DO 140 I = 1 , NUMPRT                                                   
          IF ( LD .EQ. 1 ) THEN                                                 
            PA = SQRT( PRTLST(IPX0+1,I)**2 + PRTLST(IPX0+2,I)**2 +              
     .                 PRTLST(IPX0+3,I)**2                         )            
            IF ( ABS( R - 1 ) .GT. 0.001 ) THEN                                 
              PWT(I) = PA**(R-1)                                                
            ELSE                                                                
              PWT(I) = 1.0                                                      
            END IF                                                              
            PS = PS + PWT(I)*PA                                                 
          ELSE                                                                  
            PA = SQRT( PRTLST(IPX0+1,I)**2 + PRTLST(IPX0+2,I)**2 )              
          END IF                                                                
          DO 110 LF = M1 , 1 , -1                                               
            IF ( PA .LE. PLARGE(4,LF) ) GOTO 120                                
            DO 111 J = 1 , 5                                                    
              PLARGE(J,LF+1) = PLARGE(J,LF)                                     
  111       CONTINUE                                                            
  110     CONTINUE                                                              
          LF = 0                                                                
  120     CONTINUE                                                              
          DO 130 J = 1 , 3                                                      
            PLARGE(J,LF+1) = PRTLST(IPX0+J,I)                                   
  130     CONTINUE                                                              
          PLARGE(4,LF+1) = PA                                                   
          PLARGE(5,LF+1) = PWT(I)                                               
  140   CONTINUE                                                                
                                                                                
C...FIND AND ORDER INITIAL AXES WITH HIGHEST THRUST.                            
        DO 150 LG = 1 , 11                                                      
          DIRINI(4,LG) = 0                                                      
  150   CONTINUE                                                                
        DO 210 LC = 1 , NC                                                      
          DO 160 J = 1 , 3                                                      
            PSUM(J) = 0                                                         
  160     CONTINUE                                                              
          DO 170 LF = 1 , MIN(M1,NUMPRT)                                        
            IF ( 2**LF*((LC+2**(LF-1)-1)/2**LF) .GE. LC ) THEN                  
              SGN = -PLARGE(5,LF)                                               
            ELSE                                                                
              SGN =  PLARGE(5,LF)                                               
            END IF                                                              
            DO 171 J = 1 , 4-LD                                                 
              PSUM(J) = PSUM(J) + SGN*PLARGE(J,LF)                              
  171       CONTINUE                                                            
  170     CONTINUE                                                              
          PSQ = PSUM(1)**2 + PSUM(2)**2 + PSUM(3)**2                            
          DO 180 LG = MIN(LC,10) , 1 , -1                                       
            IF ( PSQ .LE. DIRINI(4,LG) ) GOTO 190                               
            DO 181 J = 1 , 4                                                    
              DIRINI(J,LG+1) = DIRINI(J,LG)                                     
  181       CONTINUE                                                            
  180     CONTINUE                                                              
          LG = 0                                                                
  190     CONTINUE                                                              
          DO 200 J = 1 , 3                                                      
            DIRINI(J,LG+1) = PSUM(J)                                            
  200     CONTINUE                                                              
          DIRINI(4,LG+1) = PSQ                                                  
  210   CONTINUE                                                                
                                                                                
C...ITERATE DIRECTION OF AXIS UNTIL STABLE MAXIMUM                              
        LARG = 0                                                                
        LG = 0                                                                  
  220   CONTINUE                                                                
C...             LOOP FOR NEXT INITIAL DIRECTION TO ASSURE STABLE THRUST.       
C...             LARG IS # INITIAL DIRECTIONS RESULTING LARGEST THRUST.         
  225     CONTINUE                                                              
C...               LOOP FOR NEXT INITIAL DIRECTION.                             
C...               DIRINI AS STARTING DIRECTION IN (36).                        
            LG = LG + 1                                                         
            NEWTHR = 0                                                          
  230       CONTINUE                                                            
C...                 DO ITERATION (36) WHILE (NEW THRUST - OLD THRUST) < ACCUR  
              OLDTHR = NEWTHR                                                   
              IF ( NEWTHR .LE. 1E-10 ) THEN                                     
                DO 240 J = 1 , 3                                                
                  PSUM(J) = DIRINI(J,LG)                                        
                  THRDIR(J) = 0                                                 
  240           CONTINUE                                                        
              ELSE                                                              
                DO 245 J = 1 , 3                                                
                  PSUM(J) = THRDIR(J)                                           
                  THRDIR(J) = 0                                                 
  245           CONTINUE                                                        
              END IF                                                            
              DO 260 I = 1 , NUMPRT                                             
                SGN = SIGN( PWT(I), ( PSUM(1)*PRTLST(IPX0+1,I) +                
     .                                PSUM(2)*PRTLST(IPX0+2,I) +                
     .                                PSUM(3)*PRTLST(IPX0+3,I)  ) )             
                DO 250 J = 1 , 4-LD                                             
                  THRDIR(J) = THRDIR(J) + SGN*PRTLST(IPX0+J,I)                  
  250           CONTINUE                                                        
  260         CONTINUE                                                          
              NEWTHR = SQRT(THRDIR(1)**2 + THRDIR(2)**2 + THRDIR(3)**2)         
     .               / PS                                                       
            IF ( NEWTHR .GE. OLDTHR+ACCUR ) GOTO 230                            
                                                                                
C...SAVE GOOD AXIS.  TRY NEW INITIAL AXIS UNTIL A NUMBER OF TRIES AGREE.        
          IF ( NEWTHR .LT. EIGVCT(0,LD)-ACCUR .AND.  LG .LT. INIDIR )           
     .      GOTO 225                                                            
          IF ( NEWTHR .GT. EIGVCT(0,LD)+ACCUR ) THEN                            
            LARG = 0                                                            
            DO 270 J = 1 , 3                                                    
              EIGVCT(J,LD) = THRDIR(J) / ( PS*NEWTHR )                          
  270       CONTINUE                                                            
            EIGVCT(0,LD) = NEWTHR                                               
          END IF                                                                
          LARG = LARG + 1                                                       
          IF (LD .EQ. 2 .AND. (LARG .EQ. M2 .OR. LG .EQ. INIDIR)) THEN          
C           FIND MINOR AXIS AND VALUE BY ORTHOGONALITY.                         
C           IF MINOR > MAJOR, DO MORE.                                          
            EIGVCT(1,3) =-EIGVCT(2,2)                                           
            EIGVCT(2,3) = EIGVCT(1,2)                                           
            EIGVCT(3,3) = 0                                                     
            EIGVCT(0,3) = 0                                                     
            DO 290 I = 1 , NUMPRT                                               
              EIGVCT(0,3) = EIGVCT(0,3)                                         
     .                    + PWT(I)*ABS( EIGVCT(1,3)*PRTLST(IPX0+1,I)            
     .                                 +EIGVCT(2,3)*PRTLST(IPX0+2,I) )          
  290       CONTINUE                                                            
            EIGVCT(0,3) = EIGVCT(0,3) / PS                                      
            IF ( EIGVCT(0,3) .GT. EIGVCT(0,2) )                                 
     .        LARG = 1                                                          
          END IF                                                                
        IF ( LARG .LT. M2  .AND.  LG .LT. INIDIR ) GOTO 220                     
  280 CONTINUE                                                                  
                                                                                
C...EXCHANGE MAJOR AND MINOR AXES IF MINOR > MAJOR.                             
      IF ( EIGVCT(0,3) .GT. EIGVCT(0,2) ) THEN                                  
        DO 310 I = 1 , 3                                                        
          THRDIR(I) = EIGVCT(I,2)                                               
          EIGVCT(I,2) = EIGVCT(I,3)                                             
          EIGVCT(I,3) = -THRDIR(I)                                              
  310   CONTINUE                                                                
        NEWTHR = EIGVCT(0,2)                                                    
        EIGVCT(0,2) = EIGVCT(0,3)                                               
        EIGVCT(0,3) = NEWTHR                                                    
      END IF                                                                    
                                                                                
C...ROTATE BACK TO ORIGINAL COORDINATE SYSTEM.                                  
      CALL VCTROT( 2     , EIGVCT(0,2), 4     , 1   , MATRIX, -1 )              
      CALL VCTROT( NUMPRT, PRTLST     , LNGDAT, IPX0, MATRIX, -1 )              
                                                                                
      THR = EIGVCT(0,1)                                                         
      OBL = EIGVCT(0,2) - EIGVCT(0,3)                                           
                                                                                
      RETURN                                                                    
                                                                                
C-----------------------------------------------------------------------        
      ENTRY THRPRM( COMAND, PARAM, RVAL )                                       
C-----------------------------------------------------------------------        
      IF ( COMAND .EQ. 'GET' ) THEN                                             
        IF ( PARAM .EQ. 'MSTE15' ) THEN                                         
          RVAL = 10*M1 + M2                                                     
        ELSE IF ( PARAM .EQ. 'PARE31' ) THEN                                    
          RVAL = R                                                              
        ELSE IF ( PARAM .EQ. 'PARE34' ) THEN                                    
          RVAL = ACCUR                                                          
        ELSE                                                                    
          WRITE(6,*) '%ERROR IN THRPRM : BAD PARAMETER NAME. NAME = ',          
     .                PARAM                                                     
        END IF                                                                  
      ELSE IF ( COMAND .EQ. 'PUT' ) THEN                                        
        IF ( PARAM .EQ. 'MSTE15' ) THEN                                         
          IVAL = NINT(RVAL)                                                     
          M1 = IVAL / 10                                                        
          M2 = MOD(IVAL,10)                                                     
        ELSE IF ( PARAM .EQ. 'PARE31' ) THEN                                    
          R = RVAL                                                              
        ELSE IF ( PARAM .EQ. 'PARE34' ) THEN                                    
          ACCUR = RVAL                                                          
        ELSE                                                                    
          WRITE(6,*) '%ERROR IN THRPRM : BAD PARAMETER NAME. NAME = ',          
     .                PARAM                                                     
        END IF                                                                  
      ELSE                                                                      
        WRITE(6,*) '%ERROR IN THRPRM : BAD COMMAND.  COMMAND = ',               
     .              COMAND                                                      
      END IF                                                                    
      RETURN                                                                    
                                                                                
      END                                                                       
C-----------------------------------------------------------------------        
      SUBROUTINE ROTMTX( V1, V2, T )                                            
C-----------------------------------------------------------------------        
                                                                                
C...FUNCTION: CALCURATE MATRIX T TO ROTATE VECTOR V1 TO V2.                     
                                                                                
C...INPUTS:                                                                     
      REAL    V1(3), V2(3)                                                      
                                                                                
C...OUTPUTS:                                                                    
      REAL    T(3,3)                                                            
                                                                                
C...ARGUMENTS:                                                                  
C     V1  :  INITIAL DIRECTION.                                                 
C     V2  :  FINAL DIRECTION.                                                   
C              V1 AND V2 NEED NOT BE NORMARIZED.                                
C     T   :  MATRIX TO ROTATE V1 TO V2, I.E., V2 = T * V1.                      
                                                                                
C...LOCAL VARIABLES:                                                            
      REAL*8      VECT1(3), VECT2(3), UNIT1(3), UNIT2(3), XPROD(3)              
      REAL*8      SINOMG, COSOMG, V1NORM, V2NORM                                
                                                                                
C.......................................................................        
C...BODY:  PROGRAM STARTS HERE.                                                 
                                                                                
      V1NORM = 0                                                                
      V2NORM = 0                                                                
                                                                                
      DO 110 I = 1 , 3                                                          
        VECT1(I) = V1(I)                                                        
        VECT2(I) = V2(I)                                                        
        V1NORM = V1NORM + VECT1(I)**2                                           
        V2NORM = V2NORM + VECT2(I)**2                                           
        DO 100 J = 1 , 3                                                        
          T(I,J) = 0.                                                           
  100   CONTINUE                                                                
        T(I,I) = 1.                                                             
  110 CONTINUE                                                                  
                                                                                
      IF( V1NORM*V2NORM .EQ. 0 ) GOTO 8000                                      
                                                                                
      V1NORM = SQRT(V1NORM)                                                     
      V2NORM = SQRT(V2NORM)                                                     
                                                                                
      DO 120 I = 1 , 3                                                          
        UNIT1(I) = VECT1(I) / V1NORM                                            
        UNIT2(I) = VECT2(I) / V2NORM                                            
  120 CONTINUE                                                                  
                                                                                
      COSOMG = 0                                                                
      DO 130 I = 1 , 3                                                          
        COSOMG = COSOMG + UNIT1(I)*UNIT2(I)                                     
  130 CONTINUE                                                                  
                                                                                
      SINOMG = 1 - COSOMG**2                                                    
      IF ( SINOMG .LE. 0 ) THEN                                                 
        IF(COSOMG.LE.-1) THEN                                                   
          DO 140 I = 1,3                                                        
            T(I,I) = -T(I,I)                                                    
  140     CONTINUE                                                              
        END IF                                                                  
        GOTO 8000                                                               
      END IF                                                                    
      SINOMG = SQRT( SINOMG )                                                   
                                                                                
      XPROD(1) = UNIT1(2)*UNIT2(3) - UNIT1(3)*UNIT2(2)                          
      XPROD(2) = UNIT1(3)*UNIT2(1) - UNIT1(1)*UNIT2(3)                          
      XPROD(3) = UNIT1(1)*UNIT2(2) - UNIT1(2)*UNIT2(1)                          
      DO 150 I = 1 , 3                                                          
        XPROD(I) = XPROD(I) / SINOMG                                            
  150 CONTINUE                                                                  
                                                                                
      DO 160 I = 1 , 3                                                          
      DO 160 J = 1 , 3                                                          
        T(I,J) = COSOMG*T(I,J) - UNIT1(I)*UNIT2(J) + UNIT1(J)*UNIT2(I)          
     .         + (1-COSOMG)*XPROD(I)*XPROD(J)                                   
  160 CONTINUE                                                                  
                                                                                
 8000 RETURN                                                                    
      END                                                                       
C                                                                               
C                                                                               
C-----------------------------------------------------------------------        
      SUBROUTINE VCTROT( NUMPRT, PRTLST, LNGDAT, IPX0, T, INVERS )              
C-----------------------------------------------------------------------        
                                                                                
C...FUNCTION:  ROTATE VECTORS IN PRTLST USING MATRIX T.                         
                                                                                
C...INPUTS:                                                                     
      INTEGER NUMPRT, LNGDAT, IPX0, INVERS                                      
      REAL    PRTLST(LNGDAT,1), T(3,3)                                          
                                                                                
C...OUTPUTS:                                                                    
C     REAL    PRTLST(LNGDAT,1)                                                  
                                                                                
C...ARGUMENTS:                                                                  
C     NUMPRT : # PARTICLES FOR ANALYSIS FILLED IN PRTLST.                       
C     PRTLST : MOMENTUM LIST OF PARTICLES.  THREE MOMENTUM IN THIS LIST IS      
C              ROTATED BY MATRIX T.                                             
C              PRTLST(IPX0+J,I) = PX, PY, PZ OF I'TH PATICLES FOR J = 1 , 3     
C     LNGDAT : DATA LENGTH OF ONE PARTICLE IN PRTLST                            
C     IPX0   : BASE FOR THE THREE MOMENTUM IN PRTLST.                           
C              PX = PRTLST(IPX0+1), PY = PRTLST(IPX0+2), PZ = PRTLST(IPX0+3)    
C     T      : ROTATION MATRIX.  NEW VECTOR = T * OLD VECTOR. (SEE NEXT)        
C     INVERS : WHEN INVERS IS NEGATIVE, INVERSE OF T IS USED TO ROTATE VECTORS. 
C              I.E., NEW VECTOR = T**(-1) OLD VECTOR, OR OLD = T * NEW.         
C              MAXTRIX T IS ASSUMED TO BE HERMITE, T**(-1)(I,J) = T(J,I).       
                                                                                
C    LOCAL VARIABLES:                                                           
      REAL      TMPVCT(3)                                                       
                                                                                
C.......................................................................        
C...BODY: PROGRAM STARTS HERE                                                   
                                                                                
      IF ( INVERS .GE. 0 ) THEN                                                 
        DO 130 I = 1 , NUMPRT                                                   
          DO 110 J = 1 , 3                                                      
            TMPVCT(J) = 0                                                       
            DO 100 K = 1 , 3                                                    
              TMPVCT(J) = TMPVCT(J) + T(J,K)*PRTLST(IPX0+K,I)                   
  100       CONTINUE                                                            
  110     CONTINUE                                                              
          DO 120 J = 1 , 3                                                      
            PRTLST(IPX0+J,I) = TMPVCT(J)                                        
  120     CONTINUE                                                              
  130   CONTINUE                                                                
                                                                                
      ELSE                                                                      
                                                                                
        DO 230 I = 1 , NUMPRT                                                   
          DO 210 J = 1 , 3                                                      
            TMPVCT(J) = 0                                                       
            DO 200 K = 1 , 3                                                    
              TMPVCT(J) = TMPVCT(J) + T(K,J)*PRTLST(IPX0+K,I)                   
  200       CONTINUE                                                            
  210     CONTINUE                                                              
          DO 220 J = 1 , 3                                                      
            PRTLST(IPX0+J,I) = TMPVCT(J)                                        
  220     CONTINUE                                                              
  230   CONTINUE                                                                
                                                                                
      END IF                                                                    
                                                                                
      RETURN                                                                    
      END                                                                       
C                                                                               
C***********************************************************************        
C-----------------------------------------------------------------------        
      SUBROUTINE CLUSTR ( NUMPRT, PRTLST, DATLNG, IPX0,                         
     .                    NJET  , NUMJET, JETTBL, JETMOM, TGEN ,DMIN )          
C-----------------------------------------------------------------------        
C                                                                               
C...INPUTS:                                                                     
      INTEGER     NUMPRT, DATLNG, IPX0                                          
      REAL        PRTLST(DATLNG,1),     RVAL                                    
      CHARACTER*3 COMAND                                                        
      CHARACTER*6 PARAM                                                         
C     NJET AND JETMOM(1:3,1:NJET) ARE USED TO SET THE INITIAL                   
C     DIRECTIONS OF CLUSTERS WHEN MSTE(16) < 0.                                 
C                                                                               
C...OUTPUTS:                                                                    
      INTEGER     NJET, NUMJET(1), JETTBL(1)                                    
      REAL        JETMOM(4,1), TGEN, DMIN                                       
C                                                                               
C...MODIFIED VERSION OF LUCLUS IN LUND MC V.4.3                                 
C       PARTICLE DATA ARE TRANSFERED BY THE DUMMY ARGUMENTS.                    
C       PARAMETERS ARE CHANGED BY A SUBROUTINE CALL.                            
C       NO COMMON BLOCK IS USED.                                                
C       IN THE ORIGINAL PROGRAM, SOME CLUSTER ACCIDENTALLY BECOMES EMPTY        
C       WHEN MOMENTUM OF ANOTHER CLUSTER IS TOO SMALL.  IN SUCH A CASE,         
C       ZERO DIVIDE ERROR HAPPENS IN THE CALCURATION OF D2 OF CLUSTER T1        
C       AND T2 JUST AFTER DO 290 ....  IN ORDER TO AVOID SUCH A CASE,           
C       EMPTY CLUSTERS ARE PUT AWAY.                                            
C       FOR MSTE(16) < 0 AND MSTE(17) = 2, NECESSARY VARIABLE DINIT IS          
C       NOT SET IN THE ORIGINAL PROGRAM.  SO IT IS SET.                         
C       NEW OPTIONS ADDED.  MSTE(17)=3, 4                                       
C         WHEN NEW D2MIN IS LARGER THAN D2MIN IN THE PREVIOUS CONFIGURATION     
C         BY THE JUMP FACTOR FACJMP, GROUPING IS TERMINATED.                    
C         3 = 1 WITH THIS TERMINATION CONDITION.                                
C         4 = 2 WITH THIS TERMINATION CONDITION.                                
C       MODIFIED BY HIRO YAMAMOTO ON JUL. 8, 1983                               
C       A:==B MEANS VARIABLE A IN THIS PROGRAM CORESPONDS TO B IN LUCLUS.       
C                                                                               
C     SUBROUTINE LUCLUS(NJET,TGEN,DMIN)                                         
C     COMMON /LUJETS/ N,K(250,2),P(250,5)                                       
C     COMMON /LUDAT1/ MST(20),PAR(20),FPAR(40)                                  
C     COMMON /LUDATE/ MSTE(20),PARE(40)                                         
C                                                                               
C...ARGUMENTS OF SUBROUTINE CLUSTR:                                             
C     NUMPRT : # PARTICLES FOR ANALYSIS FILLED IN PRTLST. ( :== NP )            
C     PRTLST : MOMENTUM LIST OF PARTICLES.                                      
C              PRTLST(IPX0+J,I) = PX, PY, PZ OF I'TH PATICLES FOR J = 1 , 3     
C              ( PRTLST(IPX0+J,I) :== P(I',J) FOR J = 1 , 3 WHEN I AND I'       
C                CORESPONDS TO THE SAME PARTICLE. )                             
C     DATLNG : DATA LENGTH OF ONE PARTICLE IN PRTLST                            
C     IPX0   : BASE FOR THE THREE MOMENTUM IN PRTLST.                           
C              PX = PRTLST(IPX0+1), PY = PRTLST(IPX0+2), PZ = PRTLST(IPX0+3)    
C     NJET   : # JETS RECONSTRUCTED.                                            
C              IT IS THE # JETS TO BE RECONSTRUCTED WHEN MSTE(16) < 0.          
C              NJET = -1 WHEN UNSUCCESSFULL.                                    
C     NUMJET : NUMJET(I) = # PARTICLES IN I'TH JET ( :== K(N+I,2) )             
C     JETTBL : JETTBL(I) IS THE NUMBER OF THE JET TO WHICH I'TH PARTICLE IS     
C              ASSIGNED.  ( :== K(I,1) )                                        
C     JETMOM : JETMOM(J,I) = PX, PY, PZ, P OF I'TH JET FOR J = 1 , 4            
C              JETMOM IS USED TO SET THE INITIAL DIRECTIONS FOR THE ITERATION   
C              WHEN MSTE(16) < 0.                                               
C              ( :== P(N+I,J), EXCEPT THAT JETMOM(4,I) IS NOT ENERGY BUT P )    
C     TGEN   : GENERALIZED THRUST.                                              
C              SUM OF ABS( CLUSTER MOMENTUM ) / SUM OF ABS( PARTICLE MOMENTUM ) 
C     DMIN   : MINIMUM DISTANCE BETWEEN TWO JETS IN THE FINAL CONFIGURATION.    
C                                                                               
C...ARGUMENTS OF ENTRY CLUPRM:                                                  
C     COMAND : CONTROL TO GET OR PUT PARAMETER VALUE.                           
C              COMAND = 'GET' RETURNS CURRENT PARAMETER VALUE.                  
C              COMAND = 'PUT' RESETS PARAMETER VALUE.                           
C     PARAM  : IDENTIFY PARAMETER TO CHANGE.                                    
C              PARAM = 'MSTE16', 'MSTE17', 'PARE32', 'PARE33', 'PARE34'         
C                      'FACJMP'                                                 
C     RVAL   : VALUE OF MSTE(16), MSTE(17), PARE(32), PARE(33),PARE(34), FACJMP 
C              BE CAREFULL THAT ALL PARAMETERS ARE PASSED BY REAL*4 TYPE.       
C                                                                               
C...PARAMETERS:                                                                 
      INTEGER MSTE(16:17) / 2, 3 /                                              
      REAL    PARE(32:34) / 0.25, 2.5, 0.0001 /                                 
      REAL    FACJMP      / 2.0 /                                               
      REAL    D2CUT       / 1 /                                                 
      INTEGER MAXPRT, MAXJET                                                    
      PARAMETER ( MAXPRT = 200 )                                                
      PARAMETER ( MAXJET = 200 )                                                
C                                                                               
C...LOCAL VARIABLES:                                                            
      REAL    P(MAXPRT), PJET(4,MAXJET), JETVCT(4,MAXJET)                       
C                                                                               
C                                                                               
C...MAIN BODY STARTS HERE : ............................................        
C                                                                               
C...PROTECTION AGAINST TO MANY PARTICLES IN PRTLST.                             
      IF ( NUMPRT .GT. MAXPRT ) THEN                                            
        WRITE(6,*) '%ERROR IN CLUSTR : TOO MANY PARTICLES IN PRTLST. ',         
     .             'MAX = ', MAXPRT                                             
        NJET = -1                                                               
        RETURN                                                                  
      END IF                                                                    
                                                                                
C...VERY LOW MULTIPLICITIES NOT CONSIDERED                                      
      IF( NUMPRT .LE. 2*IABS(MSTE(16)) ) THEN                                   
        NJET = -1                                                               
        TGEN = -1.                                                              
        DMIN = -1.                                                              
        RETURN                                                                  
      ENDIF                                                                     
                                                                                
C...MOMENTA AND SUM OF MOMENTA FOR PARTICLES                                    
C...(P(I,5) IS TEMPORARILY USED TO REPRESENT ABSOLUTE MOMENTA)                  
      PS = 0.                                                                   
      DO 100 I = 1 , NUMPRT                                                     
        P(I) = SQRT( PRTLST(IPX0+1,I)**2 + PRTLST(IPX0+2,I)**2 +                
     .               PRTLST(IPX0+3,I)**2                         )              
        PS = PS + P(I)                                                          
  100 CONTINUE                                                                  
C                                                                               
C...  THE FOLLOWING TWO LINES ARE FOR MSTE(16) >= 0 CASE.                       
C...  FOR MSTE(16) < 0 CASE, DINIT = PARE(34).                                  
      DINIT = 1.25*PARE(32)                                                     
  105 CONTINUE                                                                  
      OLDD2M = -1                                                               
      IF(MSTE(16).GE.0) THEN                                                    
C...    FIND INITIAL JET CONFIGURATION. IF TOO FEW JETS, MAKE HARDER CUTS       
  110   DINIT = 0.8*DINIT                                                       
                                                                                
C...    SUM UP SMALL MOMENTUM REGION, JET IF ENOUGH ABSOLUTE MOMENTUM           
        NJET = 0                                                                
        NA   = 0                                                                
        DO 120 J = 1 , 3                                                        
          JETVCT(J,1) = 0                                                       
  120   CONTINUE                                                                
        DO 140 I = 1 , NUMPRT                                                   
          JETTBL(I) = 0                                                         
          IF( P(I) .LE. 2.*DINIT) THEN                                          
            NA=NA+1                                                             
            JETTBL(I) = 1                                                       
            DO 130 J = 1 , 3                                                    
              JETVCT(J,1) = JETVCT(J,1) + PRTLST(IPX0+J,I)                      
  130       CONTINUE                                                            
          END IF                                                                
  140   CONTINUE                                                                
        JETVCT(4,1) = SQRT( JETVCT(1,1)**2 + JETVCT(2,1)**2 +                   
     .                      JETVCT(3,1)**2                    )                 
        IF( JETVCT(4,1) .GT. 2.*DINIT ) NJET = 1                                
        IF( NJET+NUMPRT-NA .LT. 2*IABS(MSTE(16)) ) GOTO 110                     
                                                                                
C...    FIND FASTEST PARTICLE, SUM UP JET AROUND IT. ITERATE UNTIL ALL          
C...    PARTICLES USED UP                                                       
  150   NJET = NJET + 1                                                         
C...      PROTECTION AGAINST TOO MANY JETS RECONSTRUCTION.                      
          IF ( NJET .GT. MAXJET ) THEN                                          
            WRITE(6,*) '%ERROR IN CLUSTR : TOO MANY JETS REQUESTED ',           
     .                 'TO BE RECONSTRUCTED.  MAX = ', MAXJET                   
            NJET = -1                                                           
            RETURN                                                              
          END IF                                                                
        PMAX=0.                                                                 
        DO 160 I = 1 , NUMPRT                                                   
          IF ( JETTBL(I) .EQ. 0 .AND. P(I) .GT. PMAX ) THEN                     
            IM=I                                                                
            PMAX=P(I)                                                           
          END IF                                                                
  160   CONTINUE                                                                
        DO 170 J = 1 , 3                                                        
          JETVCT(J,NJET) = 0                                                    
  170   CONTINUE                                                                
        DO 190 I = 1 , NUMPRT                                                   
          IF ( JETTBL(I) .EQ. 0 ) THEN                                          
            D2 = ( P(I)*P(IM)                                                   
     .             - PRTLST(IPX0+1,I)*PRTLST(IPX0+1,IM)                         
     .             - PRTLST(IPX0+2,I)*PRTLST(IPX0+2,IM)                         
     .             - PRTLST(IPX0+3,I)*PRTLST(IPX0+3,IM) )                       
     .         * 2.*P(I)*P(IM) / (P(I)+P(IM))**2                                
            IF( I .EQ. IM .OR. D2 .LE. DINIT**2 ) THEN                          
              NA=NA+1                                                           
              JETTBL(I)=NJET                                                    
              DO 180 J = 1 , 3                                                  
                JETVCT(J,NJET) = JETVCT(J,NJET) + PRTLST(IPX0+J,I)              
  180         CONTINUE                                                          
            END IF                                                              
          END IF                                                                
  190   CONTINUE                                                                
        JETVCT(4,NJET) = SQRT( JETVCT(1,NJET)**2 + JETVCT(2,NJET)**2 +          
     .                         JETVCT(3,NJET)**2                       )        
        IF( NJET+NUMPRT-NA .LT. 2*IABS(MSTE(16)) ) GOTO 110                     
        IF( NA .LT. NUMPRT ) GOTO 150                                           
                                                                                
      ELSE                                                                      
C...    USE GIVEN INITIAL JET CONFIGURATION                                     
        DINIT = PARE(32)                                                        
        DO 200 IT = 1 , NJET                                                    
          DO 205 J = 1 , 3                                                      
            JETVCT(J,IT) = JETMOM(J,IT)                                         
  205     CONTINUE                                                              
          JETVCT(4,IT) = SQRT( JETVCT(1,IT)**2 + JETVCT(2,IT)**2 +              
     .                         JETVCT(3,IT)**2                     )            
  200   CONTINUE                                                                
      ENDIF                                                                     
C                                                                               
C...ASSIGN ALL PARTICLES TO NEAREST JET, SUM UP NEW JET MOMENTA                 
  210 TSAV=0.                                                                   
  220 DO 230 IT = 1 , NJET                                                      
      DO 230 J  = 1 , 3                                                         
        PJET(J,IT)=0.                                                           
  230 CONTINUE                                                                  
      DO 270 I = 1 , NUMPRT                                                     
        IF(MSTE(17).EQ.1 .OR. MSTE(17).EQ.3) THEN                               
C...      SYMMETRIC DISTANCE MEASURE BETWEEN PARTICLE AND JET                   
          D2MIN=1E10                                                            
          DO 240 IT = 1 , NJET                                                  
            IF ( JETVCT(4,IT) .GE. DINIT ) THEN                                 
              D2 = ( P(I)*JETVCT(4,IT)                                          
     .               - PRTLST(IPX0+1,I)*JETVCT(1,IT)                            
     .               - PRTLST(IPX0+2,I)*JETVCT(2,IT)                            
     .               - PRTLST(IPX0+3,I)*JETVCT(3,IT) )                          
     .           * 2.*P(I)*JETVCT(4,IT) / (P(I)+JETVCT(4,IT))**2                
              IF ( D2 .LT. D2MIN ) THEN                                         
                IM=IT                                                           
                D2MIN=D2                                                        
              END IF                                                            
            END IF                                                              
  240     CONTINUE                                                              
        ELSE                                                                    
C...      MULTICITY DISTANCE MEASURE BETWEEN PARTICLE AND JET                   
          PMAX=-1E10                                                            
          DO 250 IT = 1 , NJET                                                  
            IF ( JETVCT(4,IT) .GE. DINIT ) THEN                                 
              PROD = (   PRTLST(IPX0+1,I)*JETVCT(1,IT)                          
     .                 + PRTLST(IPX0+2,I)*JETVCT(2,IT)                          
     .                 + PRTLST(IPX0+3,I)*JETVCT(3,IT) )                        
     .             / JETVCT(4,IT)                                               
              IF ( PROD .GT. PMAX ) THEN                                        
                IM=IT                                                           
                PMAX=PROD                                                       
              END IF                                                            
            END IF                                                              
  250     CONTINUE                                                              
        ENDIF                                                                   
        JETTBL(I) = IM                                                          
        DO 260 J = 1 , 3                                                        
          PJET(J,IM) = PJET(J,IM) + PRTLST(IPX0+J,I)                            
  260   CONTINUE                                                                
  270 CONTINUE                                                                  
                                                                                
C...ABSOLUTE VALUE AND SUM OF JET MOMENTA, FIND TWO CLOSEST JETS                
      PSJT=0.                                                                   
      IT = 1                                                                    
  280 CONTINUE                                                                  
      PJET(4,IT) = SQRT( PJET(1,IT)**2 + PJET(2,IT)**2 +                        
     .                   PJET(3,IT)**2                   )                      
      IF ( PJET(4,IT) .GT. 0 ) THEN                                             
        PSJT = PSJT + PJET(4,IT)                                                
        IT = IT + 1                                                             
      ELSE                                                                      
C...    IT ACCIDENTALLY HAPPENS THAT SOME CLUSTERS BECOME EMPTY DUE TO          
C       SOME OTHER CLUSTERS HAVING SMALL MOMENTUM.                              
        NJET = NJET - 1                                                         
        IF ( NJET .LT. ABS(MSTE(16)) ) THEN                                     
          IF ( MSTE(16) .GE. 0 ) THEN                                           
            GOTO 105                                                            
          ELSE                                                                  
            WRITE(6,*) '%ERROR IN CLUSTR: COND MSTE(16) = ',MSTE(16),           
     .              ' UNACCEPTABLE.'                                            
            NJET = -1                                                           
            RETURN                                                              
          END IF                                                                
        END IF                                                                  
        DO 281 JT = IT , NJET                                                   
          DO 282 I = 1 , 3                                                      
            PJET(I,JT) = PJET(I,JT+1)                                           
  282     CONTINUE                                                              
  281   CONTINUE                                                                
      END IF                                                                    
      IF ( IT .LE. NJET ) GOTO 280                                              
                                                                                
      D2MIN=1E10                                                                
      DO 290 IT1 = 1 , NJET-1                                                   
      DO 290 IT2 = IT1+1 , NJET                                                 
        D2 = ( PJET(4,IT1)*PJET(4,IT2)                                          
     .         - PJET(1,IT1)*PJET(1,IT2)                                        
     .         - PJET(2,IT1)*PJET(2,IT2)                                        
     .         - PJET(3,IT1)*PJET(3,IT2) )                                      
     .     * 2.*PJET(4,IT1)*PJET(4,IT2) / (PJET(4,IT1)+PJET(4,IT2))**2          
        IF ( D2 .LT. D2MIN ) THEN                                               
          IM1=IT1                                                               
          IM2=IT2                                                               
          D2MIN=D2                                                              
        END IF                                                                  
  290 CONTINUE                                                                  
                                                                                
C...IF ALLOWED, JOIN TWO CLOSEST JETS AND START OVER                            
      IF( NJET .GT. IABS(MSTE(16)) .AND.                                        
     .    D2MIN .LT. PARE(33)**2         ) THEN                                 
        IF ( MSTE(17).EQ.3 .OR. MSTE(17).EQ.4 ) THEN                            
          IF ( OLDD2M .GT. 0 ) THEN                                             
            IF ( D2MIN .GE. FACJMP**2*MAX(D2CUT,OLDD2M) ) GOTO 325              
          END IF                                                                
          OLDD2M = MAX(D2MIN,OLDD2M)                                            
        END IF                                                                  
        NR=1                                                                    
        DO 300 J=1,3                                                            
          JETVCT(J,NR) = PJET(J,IM1) + PJET(J,IM2)                              
  300   CONTINUE                                                                
        JETVCT(4,NR) = SQRT( JETVCT(1,NR)**2 + JETVCT(2,NR)**2 +                
     .                       JETVCT(3,NR)**2                     )              
        DO 320 IT = 1 , NJET                                                    
          IF( IT.NE.IM1 .AND. IT.NE.IM2 ) THEN                                  
            NR=NR+1                                                             
            DO 310 J = 1 , 4                                                    
              JETVCT(J,NR) = PJET(J,IT)                                         
  310       CONTINUE                                                            
          END IF                                                                
  320   CONTINUE                                                                
        NJET=NJET-1                                                             
        GOTO 210                                                                
      ENDIF                                                                     
  325 CONTINUE                                                                  
                                                                                
C...IF GENERALIZED THRUST HAS NOT YET CONVERGED, CONTINUE ITERATION             
      TGEN = PSJT / PS                                                          
      IF( TGEN .GT. TSAV+PARE(34) ) THEN                                        
        TSAV = TGEN                                                             
        DO 330 IT = 1 , NJET                                                    
        DO 330 J = 1 , 4                                                        
          JETVCT(J,IT) = PJET(J,IT)                                             
  330   CONTINUE                                                                
        GOTO 220                                                                
      ENDIF                                                                     
                                                                                
C...REORDER JETS AFTER MOMENTUM, SUM UP JET ENERGIES AND MULTIPLICITIES         
      DO 350 IT = 1 , NJET                                                      
        PMAX=0.                                                                 
        DO 340 IR = 1 , NJET                                                    
          IF ( PJET(4,IR) .GT. PMAX ) THEN                                      
            IM   = IR                                                           
            PMAX = PJET(4,IR)                                                   
          END IF                                                                
  340   CONTINUE                                                                
C...    NUMJET(IM) = THE NEW POSITION OF THE JET WHICH IS IN THE IM'TH          
C                    POSITION BEFORE P ORDERING.  TEMPORALLY USED.              
        NUMJET(IM) = IT                                                         
        PJET(4,IM) = 0                                                          
        DO 350 J = 1 , 3                                                        
          JETMOM(J,IT) = PJET(J,IM)                                             
  350 CONTINUE                                                                  
      DO 360 I = 1 , NUMPRT                                                     
        JETTBL(I) = NUMJET(JETTBL(I))                                           
  360 CONTINUE                                                                  
      DO 361 I = 1 , NJET                                                       
        NUMJET(I) = 0                                                           
  361 CONTINUE                                                                  
      DO 362 I = 1 , NUMPRT                                                     
        NUMJET(JETTBL(I)) = NUMJET(JETTBL(I)) + 1                               
  362 CONTINUE                                                                  
      DO 370 IT = 1 , NJET                                                      
        JETMOM(4,IT) = SQRT( JETMOM(1,IT)**2 + JETMOM(2,IT)**2 +                
     .                       JETMOM(3,IT)**2                     )              
  370 CONTINUE                                                                  
C                                                                               
      DMIN=SQRT(D2MIN)                                                          
      IF(NJET.EQ.1) DMIN=0.                                                     
      RETURN                                                                    
C                                                                               
      ENTRY CLUPRM( COMAND, PARAM, RVAL )                                       
      IF ( COMAND .EQ. 'GET' ) THEN                                             
        IF ( PARAM .EQ. 'MSTE16' ) THEN                                         
          RVAL = MSTE(16)                                                       
        ELSE IF ( PARAM .EQ. 'MSTE17' ) THEN                                    
          RVAL = MSTE(17)                                                       
        ELSE IF ( PARAM .EQ. 'PARE32' ) THEN                                    
          RVAL = PARE(32)                                                       
        ELSE IF ( PARAM .EQ. 'PARE33' ) THEN                                    
          RVAL = PARE(33)                                                       
        ELSE IF ( PARAM .EQ. 'PARE34' ) THEN                                    
          RVAL = PARE(34)                                                       
        ELSE IF ( PARAM .EQ. 'FACJMP' ) THEN                                    
          RVAL = FACJMP                                                         
        ELSE                                                                    
          WRITE(6,*) '%ERROR IN CLUPRM : BAD PARAMETER NAME. NAME = ',          
     .                PARAM                                                     
        END IF                                                                  
      ELSE IF ( COMAND .EQ. 'PUT' ) THEN                                        
        IF ( PARAM .EQ. 'MSTE16' ) THEN                                         
          MSTE(16) = NINT(RVAL)                                                 
        ELSE IF ( PARAM .EQ. 'MSTE17' ) THEN                                    
          MSTE(17) = NINT(RVAL)                                                 
        ELSE IF ( PARAM .EQ. 'PARE32' ) THEN                                    
          PARE(32) = RVAL                                                       
        ELSE IF ( PARAM .EQ. 'PARE33' ) THEN                                    
          PARE(33) = RVAL                                                       
        ELSE IF ( PARAM .EQ. 'PARE34' ) THEN                                    
          PARE(34) = RVAL                                                       
        ELSE IF ( PARAM .EQ. 'FACJMP' ) THEN                                    
          FACJMP = RVAL                                                         
        ELSE                                                                    
          WRITE(6,*) '%ERROR IN CLUPRM : BAD PARAMETER NAME. NAME = ',          
     .                PARAM                                                     
        END IF                                                                  
      ELSE                                                                      
        WRITE(6,*) '%ERROR IN CLUPRM : BAD COMMAND.  COMMAND = ',               
     .              COMAND                                                      
      END IF                                                                    
      RETURN                                                                    
      END                                                                       
