C   08/01/87 804201909  MEMBER NAME  @FLAGRD  (FORT)     M  FORTRAN
C+
C (Filename) @FLAGRD
C (Author  ) A.Shirahashi, Univ. of Tokyo
C (Date    ) 8-Jan-1987
C (History ) 27-May-87, Add ICON
C             1-Dec-87, Add IEOF
C-
       COMMON /FLAGRD/ IFLQ, IFCR, ICOM, ILOG, INHLOG,
     &                 IPNT, IFF, ICTZ, IDEBUG, ICON, ISNGL, LOPNRD
C
       INTEGER * 4  IFLQ, IFCR, ICOM, ILOG, INHLOG, IPNT, IFF, ICTZ
       LOGICAL * 4  IDEBUG, LOPNRD
       INTEGER * 4  ICON, ISNGL
C
