      SUBROUTINE TERMID(NAME)
      CHARACTER*8  NAME
      CHARACTER*20 ENV
#ifdef HIUXF77
      call getenv('DISPLAY',7, ENV, 20)
#else
      CALL GETENV('DISPLAY',ENV)
#endif
      NAME=ENV(1:8)
      RETURN
      END
