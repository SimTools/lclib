/*
  File : fd_to_fileno.c
  Description : Convert file descripter to file number

  Author : Ryosuke Itoh, TOPAZ, KEK
  Date : 9 - NOV - 1991
*/
#include <stdio.h>

void fd_to_fileno_ ( fd, fn )
/* arg */
FILE **fd;
int *fn;
{
/* main */
  *fn = fileno ( *fd );
}
