/*
  strupc for ghf77 on AViiON
*/

#include <string.h>

strupc_( n, s, l )
int *n;
char *s;
int l;
{
  int i;

   for( i = 0; i < *n && i < l ; ++i, ++s ) {
     *s = toupper( *s );
   }  
}
