COPTIONS @OPTIONS  NODEBUG
C   18/11/86 704142059  MEMBER NAME  FEXSUB   (FORT)     M  FORTRAN
C+
C File : FEXSUB
C Description : Execute specified subroutine
C
C Author : Ryosuke Itoh, Univ. of Tokyo
C Date : 17 - NOV - 1986
C-
C
        SUBROUTINE FEXSUB ( ADRS )
C
C ARGUMENTS
C
        EXTERNAL        ADRS
C
C MAIN
C
        CALL ADRS
C
        RETURN
        END
