C   14/04/87 704150100  MEMBER NAME  @FLLEVEL (FORT)     M  FORTRAN
C+
C File : @FLLEVEL
C Description : Common to store execution/debug levels
C
C Author : Ryosuke Itoh, Univ. of Tokyo
C Date : 14 - APR - 1987
C-
C
      INTEGER * 4      MAXLVL
      PARAMETER        ( MAXLVL = 64 )
      COMMON /FLLEVL/  EXELVL(MAXLVL), DBGLVL(MAXLVL),
     #                 DBGRNG(2,MAXLVL)
      INTEGER * 4      EXELVL, DBGLVL
      INTEGER * 4      DBGRNG
C
C E N D
C
