c+
c File : tbsetf.F
c Description : Set Conversion format for TBS I/O routines
c
c Author : Ryosuke Itoh, Physics Division, KEK
c Date : 13 - Dec - 1994
c-
c
      Subroutine TBSETF ( Address, Flag )
c
c Args
c
      Integer * 4      Address, Flag
c                        where
c                         Flag = 0 ; no conversion
c                                  1 ; Use local VBS file
c                                  2 ; Use RMTL
c                                  3 ; Use SONY DIR-1000
c                        and
c                         Address = 1 : for TBREAD
c                                   2 : for TBWRIT
c                                   3 : for TBCRED
c                                   4 : for TBCWRT
c
c Common
c
#include "tbsfmt.inc"
c
c Main
c
      NRWFMT ( Address ) = Flag
      Return
      End
