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
void tb_set_buffer__ ( lun, mxsize, iwork )

/* arg */
int *lun;
int *mxsize;
int *iwork;

{
/* var */
  extern struct buf buffer;

  int fn;
  int is;

/* func */
  void lun_to_fileno_();

/* main */
  lun_to_fileno_ ( lun, &fn );
  
  printf( "In tb_set_buffer: lun = %d fn = %d\n", *lun, fn );
  
  buf.fd = fn;
  buf.top = iwork;
  buf.pntr = buf.top;
  buf.size = *mxsize;
  buf.lock =0;
  buf.nc = 0;
  buf.ntot = buf.nc;
  
  printf( "iwork=%d top=%d maxsize=%d lock=%d nc=%d ntot=%d\n",
  iwork,buf.top,*mxsize,buf.lock,buf.nc,buf.ntot);
  
}

/*----------
  tb_write_buffer
  ----------*/
void tb_write_buffer__ ( nw, data )

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
  
  printf( "iwork=%d top=%d maxsize=%d lock=%d nc=%d ntot=%d\n",
  buf.top,buf.top,buf.size,buf.lock,buf.nc,buf.ntot);
  
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
void tb_flush_buffer__( ntot )

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
    if (is) printf("write returns is = %d\n",is);
  }
  if ( buf.lock == 1 )
    is = fcntl ( buf.fd, FCNTL_FLOCK, 0 );

  *ntot = buf.ntot;

  printf( "top=%d pntr=%d maxsize=%d lock=%d nc=%d ntot=%d\n",
  buf.top,buf.pntr,buf.size,buf.lock,buf.nc,buf.ntot);
  
}
/*----------
  tb_read_buffer
  ----------*/
int tb_read_buffer__ ( lun, nw, buf )

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
  
  printf("read returns is = %d\n",is);
  
  if ( is<=0 ) {
    return ( 0 );
  }
  else
    return ( 1 );
}

