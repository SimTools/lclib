/* UVSETC    ECSECT  */
/***************************************************************/
/*                                                             */
/* SUBROUTINE UVSETC( IADD, STRING, LBYTE)                     */
/*                                                             */
/*  (Purpose)                                                  */
/*       Copy character in STRING to the address start from    */
/*       IADD.                                                 */
/*  (Input)                                                    */
/*       IADD  : Start address where STRING is copied.         */
/*       STRING: Source data.                                  */
/*       LBYTE : # of byte of source.                          */
/*  (Output)                                                   */
/*       The content of the location IADD is set equal to STRING*/
/*  (Author)                                                   */
/*       A. Miyamoto    13-Jun-1986                            */
/*       A. Miyamoto    14-Apr-1992 Converted to C for HP      */
/***************************************************************/
/*                          */
uvsetc_(iaddr, val, lbyte)  
        int *iaddr,*lbyte ;
        char *val ; 
{
	char *naddr ;
	int  i      ;
	naddr = * iaddr ; 
        for ( i=0 ; i<*lbyte ; i++ )
{	*(naddr+i) = *(val+i) ; }  ;
	}


