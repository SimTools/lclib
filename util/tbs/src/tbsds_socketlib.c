/*
** setup_client.c  utility program
*/

#include <stdio.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <netdb.h>
/* #include <stropts.h> */

#include "tbsds_iocntl.h"


int tbsds_init_server_(hostname, toserver, fromserver, iret)
char *hostname;
int *toserver, *fromserver;
int *iret;
{
  extern struct tbsds_iocntl tbsds_iocntl;

  tbsds_iocntl.fd_receive = tbsds_server_socket_(hostname, toserver);
  if( tbsds_iocntl.fd_receive == -1 ) {
    *iret = -1 ;
    return;
  }
  tbsds_iocntl.fd_send = tbsds_server_socket_(hostname, fromserver);
  if( tbsds_iocntl.fd_send ==-1 ) {
    *iret = -1 ;
    return;
  }
  *iret = 0;
  FD_ZERO(&tbsds_iocntl.mask_rcv);
  FD_ZERO(&tbsds_iocntl.mask_send);
  FD_SET(tbsds_iocntl.fd_receive, &tbsds_iocntl.mask_rcv);
  FD_SET(tbsds_iocntl.fd_send, &tbsds_iocntl.mask_send);


}



int tbsds_init_client_(hostname, toserver, fromserver, iret)
char *hostname;
int *toserver, *fromserver;
int *iret;
{
  extern struct tbsds_iocntl tbsds_iocntl;

  tbsds_iocntl.fd_send = tbsds_client_socket_(hostname, toserver);
  if( tbsds_iocntl.fd_send == -1 ) {
    *iret = -1 ;
    return;
  }
  tbsds_iocntl.fd_receive = tbsds_client_socket_(hostname, fromserver);
  if( tbsds_iocntl.fd_receive ==-1 ) {
    *iret = -1 ;
    return;
  }
  *iret = 0;
  FD_ZERO(&tbsds_iocntl.mask_rcv);
  FD_ZERO(&tbsds_iocntl.mask_send);
  FD_SET(tbsds_iocntl.fd_receive, &tbsds_iocntl.mask_rcv);
  FD_SET(tbsds_iocntl.fd_send, &tbsds_iocntl.mask_send);


}


int tbsds_client_socket_(hostname, lport)
char *hostname;
long *lport;
{
 	struct hostent *servhost;
	struct sockaddr_in server;
	int s;
        u_short port;
  
        port = *lport;
	printf(" port =%d ",port);
	if((servhost = gethostbyname(hostname)) == NULL)
		{
		printf("bad hostname\n");
		return -1;
		}

	bzero((char *)&server, sizeof(server));
	server.sin_family = AF_INET;
	server.sin_port = port;
	bcopy(servhost->h_addr, (char *)&server.sin_addr, 
		servhost->h_length);
	if((s=socket(AF_INET,SOCK_STREAM,0)) < 0 ) 
		{
		printf("socket allocation failed.\n");
		return -1;
		}

	if(connect(s,&server,sizeof(server)) ==-1)
		{
		printf("cannot connect.\n");
		return -1;
		}
	printf(" connected.\n");
	return s;
}


int tbsds_server_socket_(hostname, lport)
char *hostname;
long *lport;
{
	struct hostent *myhost;
	struct sockaddr_in me;
	int s,s_waiting;
        u_short port;

        port = *lport;
	printf(" port =%d ",port);

	if((myhost = gethostbyname(hostname)) == NULL)
		{
 		printf("bad hostname!\n");
		return -1;
		}

	bzero((char*)&me,sizeof(me));
	me.sin_family = AF_INET;
	me.sin_port = port;
	bcopy(myhost->h_addr, (char*)&me.sin_addr, myhost->h_length);
	if((s_waiting=socket(AF_INET,SOCK_STREAM,0)) < 0 ) 
		{
		printf("socket allocaion failed.\n");
		return -1;
		}

	if (bind(s_waiting, &me, sizeof(me)) == -1)
		{
		printf("cannot bind.\n");
		return -1;
		}

	printf("successfully bound, now listens.\n");

	listen(s_waiting,1);
	s = accept(s_waiting, NULL, NULL);
	close(s_waiting);

	return s;
}


void tbsds_end_()
{
  close(tbsds_iocntl.fd_receive);
  close(tbsds_iocntl.fd_send);
  printf("tbsds sockets were closed.\n");
}
/* =================================================================
**
**  Subroutine tbsds_send(nw, buf, nret )
** (Function)
**  send data to server(client)
** (Input)
**  nw : size of buf, which data transfered to the recipiant.
**  buf: data to be transfered.
** (Output)
**  nret: Return code of transfer.
**      >= 0 ; when write sucessfull.
**       =-10 ; Quit write request received from recipiant.
**       =-11 ; Invalid read data from recipiant
**       = -1 ; write error.
**
** =================================================================
*/
void tbsds_send_(nw, buf, nret)
int *nw;
int buf[];
int *nret;
{
   int lread;
   int iw;
   int kw;
   fd_set writeok, readok;
   int isel;

   iw = *nw*4;
   writeok = tbsds_iocntl.mask_send ;
   readok = tbsds_iocntl.mask_rcv ;
#ifdef TEST
   printf("Going to write \n");
#endif
   for (;;) {
     isel=select(tbsds_iocntl.fd_receive+2, (fd_set *)&readok, (fd_set *)&writeok, NULL, NULL); 
     
/*     isel=select(tbsds_iocntl.fd_receive+2, NULL, (fd_set *)&writeok, NULL, NULL); */
/*     printf(" isel at send is %d \n",isel); */

/*
** Read data when it exists.
*/
     if ( FD_ISSET(tbsds_iocntl.fd_receive, &readok) ){
       kw = read(tbsds_iocntl.fd_receive, buf, 4);
       if( buf[0] == 0 ) {
	 printf("Got request to quit while sending data in tbsds_send\n");
	 *nret = -10;
	 return;
       }
       else {
	 printf("Got invalid data while sending the data.\n");
	 printf("Obtained data was kw =%d buf=%d \n",kw,buf[0]);
	 *nret = -11;
	 return;
       }
     }
/*
** Send data when no data is obtained.
*/
     if ( FD_ISSET(tbsds_iocntl.fd_send, &writeok ) ){
/*       printf(" Going to write ... nw=%d \n",*nw); */
       kw = write(tbsds_iocntl.fd_send, nw, 4) ;
       if( kw < 0 ) {
	 printf("Write error in tbsds_send  kw =%d iw=%d \n", kw, iw);
	 *nret = kw;
	 return;
       }
/*       printf("Going to write... iw=%d",iw); */
       kw=write(tbsds_iocntl.fd_send, buf, iw); 
       if( kw < 0 ) {
	 printf("Write error in tbsds_send  kw =%d iw=%d \n", kw, iw);
       }
       *nret = kw;
        return;
     }

   }
 }

/* =================================================================
**
**  Subroutine tbsds_receive(maxbuf, nw,  buf )
** (Function)
**  Read data from sender.
** (Input)
**  maxbuf : size of data buffer.
** (Output)
**  nw : Data size. 
**     = 0 eof
**     < 0 when error is detected.
**  buf : data buffer
**
** =================================================================
*/
#ifdef PPC
void tbsds_receive__(maxbuf, nw, cbuf)
#else
void tbsds_receive_(maxbuf, nw, cbuf)
#endif
long *maxbuf;
long *nw;
char cbuf[];
{
   long lread;
   fd_set readok;
   int isel;
   int maxbyte;
   long *readbuf;

   int lbufs,iret, ip, lastbyte;

   maxbyte = *maxbuf * 4;
   
   readok = tbsds_iocntl.mask_rcv ;
   *nw = 0;
#ifdef TEST
   printf("Start receive.\n");
#endif
  for (;;) {
     isel=select(tbsds_iocntl.fd_receive+1, (fd_set *)&readok, NULL, NULL, NULL);
/*     printf(" isel at rcv is %d \n",isel); */
     if ( FD_ISSET(tbsds_iocntl.fd_receive, &readok ) ){
       if( *nw == 0 ) {
	  lbufs = 4;
          lread = read(tbsds_iocntl.fd_receive, nw, lbufs );
	  if( lread < 0 ) {
	    printf("Error to read first word in tbsds_receive_.\n");
	    *nw = -1;
	    return ;
	  }
/*	  printf("lread=%d",lread);  */
	  lastbyte = *nw*4 ;
	  ip = 0;
	  lbufs = lastbyte;
/*	  printf("Going to read nw=%d \n",*nw); */
          if ( *nw == 0 ) return;
	}
       else {
         lread = read(tbsds_iocntl.fd_receive, &cbuf[ip], lbufs ); 
	 if( lread < 0 ) {
	    printf("Error to read data in tbsds_receive_.\n");
	    *nw = -1;
	    return ;
	  }
/*	 printf(" lread=%d, ip=%d, lastbyte=%d\n",lread, ip, lastbyte);  */
         ip = ip + lread;
         if( ip == lastbyte ) return ;
	 if( ip > lastbyte ) {
	   printf("Read more than required data ip =%d lastbyte=%d \n",ip,lastbyte);
	   return;
	 }
         lbufs = lastbyte - ip;
/*         printf("lbufs, maxbyte, ip =%d %d %d \n",lbufs, maxbyte, ip); */
	 if( lbufs < 1 ) {
	    printf("Error to read data in tbsds_receive_.");
	    printf("Data size exceeds buffer size.\n");
	    *nw = -1;
	    return ;
	 }
       }
    }
   }
 }




