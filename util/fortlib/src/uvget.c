/*UVGET     ECSECT*/
/***************************************************************/
/*                                                             */
/* SUBROUTINE UVGET( NBYTE, IADD , ARRAY )                     */
/*                                                             */
/*  (Purpose)                                                  */
/*       Copy the contents of memory from IADD by NBYTES       */
/*       to the ARRAY.                                         */
/*  (Input)                                                    */
/*       IADD  : Start address.                                */
/*       NBYTE : # of bytes to copy.                           */
/*  (Output)                                                   */
/*       ARRAY : Copy destination.                             */
/*  (Author)                                                   */
/*       A. Miyamoto    Sep.25,'87                             */
/*       A. Miyamoto    Feb. 8,'92 Converted to C to run on HP */
/*       A. Miyamoto    May,'4,'94 Converted to C to run on HP */
/***************************************************************/

uvget_(nbyte, iaddr, array)  
int *nbyte ; 
int *iaddr,*array ;
{ 
	int *naddr, i    ; 
	short *from, *to ;
	from = *iaddr ;
	to   = array ;
	for(i=1;i<=*nbyte;i++)
	  { *to    = *from  ;
	     to += 1 ;
	     from +=1 ; } ; 
	}
