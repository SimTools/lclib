#ifdef HIUXF77
      INTEGER FUNCTION GETCWD(DSN,LDSN)
      CHARACTER*(*) DSN
      INTEGER*4     LDSN
      call getenv( 'PWD', 3, DSN, LDSN)
#else
      INTEGER FUNCTION GETCWD_(DSN,LDSN)
      CHARACTER*(*) DSN
      INTEGER*4     LDSN
      GETCWD_ = GETCWD(DSN,LDSN)
#endif
      RETURN
      END
