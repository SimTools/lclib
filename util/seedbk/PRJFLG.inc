CC********************************************************************CC
C*                                                                    *C
C*=================                                                   *C
C*   / PRJFLG /                                                       *C
C*=================                                                   *C
C*                                                                    *C
C* 1) Contents                                                        *C
C*     This COMMON contains the default level and debug flag of the   *C
C*     job.                                                           *C
C*                                                                    *C
C*                                                                    *C
C*                                                                    *C
C* 2) Update Record                                                   *C
C*    10/14/85  A. Miyamoto  Original Version.                        *C
C*    10/22/87  K. Fujii     Levl = 2 for LUM reduction.              *C
C*    10/26/87  K. Fujii     Levl = 2 for BTOF production.            *C
C*     4/25/89  A. Miyamoto  Output execution summary as default.     *C
C*     2/19/91  K. Fujii     Level = 2 for FCL reduction.             *C
C*                                                                    *C
CC********************************************************************CC
      COMMON /PRJFLG/ NJBLVL(30, 3), NJBDBG(30, 3)
C
C*** For Production job.
C
C=    DATA (NJBDBG(K,1),K=1,30) /0, 8, 28*0/
C=    DATA (NJBLVL(K,1),K=1, 5)
C=   ./  1,  3, 3*0/
C=    DATA (NJBLVL(K,1),K=6, 20)
C      IDC,TPC,BTOF,BDC,BCL,BMU,ETOF,EDC,ECL,LUM, *,HTRK,HNEU,HFIL,USR
C=   ./  3,  2,   2,  1,  1,  1,  0,   1,  1,  0, 0,   1,   1,   1,  1/
C=    DATA (NJBLVL(K,1),K=21,30)
C      VTX,TCH,FCL,RCL, dummy
C=   ./  1,  1,   1,  1,  6*0/
C
C
C
C*** For Reduction job..
C
C=    DATA (NJBDBG(K,2),K=1,30) /30*0/
C=    DATA (NJBLVL(K,2),K=1, 5)
C=   ./  1,  1, 3*0/
C=    DATA (NJBLVL(K,2),K=6, 20)
C      IDC,TPC,BTOF,BDC,BCL,BMU,ETOF,EDC,ECL,LUM, *,FLT,
C=   ./  2,  2,   2,  0,  2,  2,  0,   2,  2,  2, 0,  1 ,   0,   0,  0/
C=    DATA (NJBLVL(K,2),K=21,30)
C      VTX,TCH,FCL,RCL, dummy
C=   ./  1,  1,   2,  1,  6*0/
C
C*** For simulator.
C=    DATA (NJBDBG(K,3),K=1,30) /30*0/
C=    DATA (NJBLVL(K,3),K=1,30) /30*0/
C
