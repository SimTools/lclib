C* UVCPYH    ECSECT
C***************************************************************
C*                                                             *
C* SUBROUTINE UVCPYH( NWORD, FROM, TO )                        *
C*                                                             *
C*  (Purpose)                                                  *
C*       Copy Array                                            *
C*  (Input)                                                    *
C*       NWORD : # of words ( size of ARRAY. in I*2 Unit)      *
C*       FROM  : Source array.                                 *
C*       TO    : Destination array.                            *
C*  (Author)                                                   *
C*       A. Miyamoto    17-Sep-1986                            *
C*       A. Miyamoto     8-Feb-1992 Prepared FORTRAN version   *
C*				    for use on HP	       *
C*       A. Miyamoto     4-May-1994 for SUN                    *
C***************************************************************

	SUBROUTINE UVCPYH( NWORD, FROM, TO )
	INTEGER*2  FROM(NWORD), TO(NWORD)
	DO 100 I = 1, NWORD
	  TO(I) = FROM(I)
100	CONTINUE
CC#ifdef  HP
CC         call vec_xicopy16(from, to, nword)
CC#endif
	RETURN
	END
