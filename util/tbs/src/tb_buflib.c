/*
  File : tb_buflib.c
  Description : do direct file_io using write and read

  Author : Ryosuke Itoh, TOPAZ, KEK
  Date : 23 - JAN - 1990
*/
#include <stdio.h>

#define FCNTL_FLOCK 1001

struct buffer {
  int *top;
  int size;
  int fd;
  int *pntr;
  int nc;
  int ntot;
  int lock;
};

struct buffer buf;

/*----------
  tb_set_buffer
  ----------*/
void tb_set_buffer_ ( lun, mxsize, iwork )
/* arg */
int *lun;
int *mxsize;
int *iwork;

{
/* var */
  extern struct buffer buf;

  int fn;
  int is;

/* func */
  void lun_to_fileno_();

/* main */
  lun_to_fileno_ ( lun, &fn );
  
/*  printf( "lun = %d fn = %d\n",lun*, fn ); */
  
  buf.fd = fn;

  buf.top = iwork;
  buf.pntr = buf.top;
  buf.size = *mxsize;
  buf.lock =0;
  buf.nc = 0;
  buf.ntot = buf.nc;
}

/*----------
  tb_write_buffer
  ----------*/
void tb_write_buffer_ ( nw, data )
/* arg */
int *nw;
int *data;

{
/* var */
  extern struct buffer buf;
  int i;
  int is;
  int nwds;

/* main */
  nwds = *nw;
  for ( i=0;i<nwds;i++ ) {
    *buf.pntr++ = *data++;
    buf.nc++;
    if ( buf.nc >= buf.size ) {
      if ( buf.lock == 0 ) {
	is = fcntl ( buf.fd, FCNTL_FLOCK, 1 );
	buf.lock = 1;
      }
      is = write ( buf.fd, buf.top, buf.nc*4 );
      buf.pntr = buf.top;
      buf.ntot = buf.ntot + buf.nc;
      buf.nc = 0;
    }
  }
}
    
/*----------
  tb_flush_buffer
  ----------*/
void tb_flush_buffer_( ntot )
/* arg */
int *ntot;

{
/* var */
  extern struct buffer buf;
  int is;

/* main */
  if ( buf.nc > 0 ) {
    is = write ( buf.fd, buf.top, buf.nc*4 );
    buf.ntot = buf.nc;
  }
  if ( buf.lock == 1 )
    is = fcntl ( buf.fd, FCNTL_FLOCK, 0 );

  *ntot = buf.ntot;
}
/*----------
  tb_read_buffer
  ----------*/
int tb_read_buffer_ ( lun, nw, buf )
/* arg */
int *lun;
int *nw;
int *buf;

{
/* var */
  int fn;
  int is;

/* func */
  void lun_to_fileno_();

/* main */
  lun_to_fileno_ ( lun, &fn );
  is = read ( fn, nw, 4 );
  if ( is <= 0 ) return ( 0 );
  if ( *nw == 0 ) return ( 1 );
  is = read ( fn, buf, *nw*4 );
  if ( is<=0 ) {
    return ( 0 );
  }
  else
    return ( 1 );
}

