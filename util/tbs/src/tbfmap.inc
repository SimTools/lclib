C   10/07/89 907102323  MEMBER NAME  @VAXMAP  (FORT)     M  FORTRAN
C+
C File : @TBFMAP
C Description : INternal Common for Bank translation
C
C Author : Ryosuke Itoh, TOPAZ, KEK
C Date : 10 - JUL - 1989
C Original name of VAXMAP was modified to TBFMAP by A.Miyamoto 17/May/94
C-
C
      INTEGER * 4      MAXBNK, MAXELM, MAXRNG
      PARAMETER        ( MAXBNK = 50 )
      PARAMETER        ( MAXELM = 200 )
      PARAMETER        ( MAXRNG = 10  )
C
      COMMON / TBFMAP / NMAP, MAPID(MAXBNK), MAPBNK(MAXBNK),
     #                  MAPMEL(MAXBNK), MAPELM(MAXBNK,MAXELM),
     #                  NRANGE(MAXBNK,MAXELM),
     #                  RANGE(MAXBNK,MAXELM,MAXRNG,2),
     #                  MAPTYP(MAXBNK,MAXELM,MAXRNG)
C
      INTEGER * 2      NMAP, MAPID , MAPMEL, MAPELM
      CHARACTER * 64   MAPBNK
      INTEGER * 2      NRANGE, RANGE, MAPTYP
