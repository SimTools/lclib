/* UVSET     ECSECT
/***************************************************************/
/*                                                             */
/* SUBROUTINE UVSET( IADD, VAL )                               */
/*                                                             */
/*  (Purpose)                                                  */
/*       Set memory of address IADD equal to the value VAL     */
/*  (Input)                                                    */
/*       IADD  : Address of memory                             */
/*       VAL   : The value to be set                           */
/*  (Output)                                                   */
/*       The content of the location IADD is set equal to VAL  */
/*  (Author)                                                   */
/*       S.Kawabata     Nov.22 '85                             */
/*       A.Miyamoto     Feb.7 '92 Converted to C to run on HP  */
/***************************************************************/
uvset_(iaddr, val)  int *iaddr,*val ;
{
	int *naddr ;
	naddr = *(iaddr) ;
	*naddr = *(val) ;
}


