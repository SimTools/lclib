/* NUADDR    ECSECT */
/***************************************************************/
/*                                                             */
/* INTEGER FUNCTION NUADDR( I )                                */
/*                                                             */
/*  (Purpose)                                                  */
/*       Get address of the input variable                     */
/*  (Input)                                                    */
/*       I     : Input variable of which arrdess is asked      */
/*  (Output)                                                   */
/*       NUADDR: Address of the variable                       */
/*  (Author)                                                   */
/*       S.Kawabata     Nov.22 '85                             */
/*       A.Miyamoto     Feb.7 '92  Converted to C to run on HP */
/***************************************************************/

nuaddr_(x) int *x ;{
	int iadr ;
	iadr = (int)x ;
	return(iadr) ;
}
