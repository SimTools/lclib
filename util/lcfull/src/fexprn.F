COPTIONS @OPTIONS  NODEBUG
C   18/11/86 704180129  MEMBER NAME  FEXPRN   (FORT)     M  FORTRAN
C+
C File : FEXPRN
C Description : Execute specified subroutine
C
C Author : Ryosuke Itoh, Univ. of Tokyo
C Date : 17 - NOV - 1986
C-
C
        SUBROUTINE FEXPRN ( ADRS, LEVEL, DEBUG )
C
C ARGUMENTS
C
        EXTERNAL        ADRS
        INTEGER * 4     LEVEL, DEBUG
C
C MAIN
C
        CALL ADRS ( LEVEL, DEBUG )
C
        RETURN
        END
