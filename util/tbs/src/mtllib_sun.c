/* ************************************************************************* */
/*  Remote MTL access system : RMTL liblary                                  */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/* ************************************************************************* */
/*  Liblary  List                                                            */
/*                                                                           */
/*  - mtlint_()                                                               */
/*    ��ѥ���ֳ����׵����                                                 */
/*  - mtlatr_()                                                               */
/*    ���ȥ�ӥ塼���׵����                                                 */
/*  - mtlalc_()                                                               */
/*    ���̣̣ϣ��׵��������������                                           */
/*  - mtlopn_()                                                               */
/*    �ϣУţ��׵����                                                       */
/*  - mtlred_()                                                               */
/*    �ңţ����׵��������������                                             */
/*  - mtlwrt_()                                                               */
/*    �ףңɣԣ��׵����������                                               */
/*  - mtlctl_()                                                               */
/*    ����ȥ����׵����������                                             */
/*  - mtlcls_()                                                               */
/*    �ạ̃ϣӣ��׵����������                                               */
/*  - mtlfre_()                                                               */
/*    �ƣңţ��׵������������                                               */
/*  - mtlend_()                                                               */
/*    ��ѥ���ֽ�λ�׵������                                               */
/*                                                                           */
/* ************************************************************************* */
/* ***  �����ѹ�  94.05.24  start ****************************************** */
/*  1.  ��ά���ɲãģ�ʸ�ɲ�                                                 */
/*      //STDERR  DD  SYSOUT=*                                               */
/* ***  �����ѹ�  94.05.24  end   ****************************************** */

/* ***  �����ѹ�  94.06.18  start ****************************************** */
/*  1.  �����ॢ����ȯ�����Υ����åȥ����������ɲ�                         */
/*  2.  ALLOC�ʳ����Ԥ���碌���֤��Ĺ��������->��������                    */
/* ***  �����ѹ�  94.06.18  end   ****************************************** */

/* ***  �����ѹ�  94.10.24  start ****************************************** */
/*  1.  �ؿ�SND_data�ˤ����롢send�ؿ��������������������������������������� */
/*      send( sok, data, datalen ) -> send( sok, data, datalen ,0 )          */
/* ***  �����ѹ�  94.10.24  end   ****************************************** */

#include <sys/types.h>
#include <sys/time.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <string.h>
#include <netdb.h>
#include <netinet/in.h>
#include <ctype.h>
#include <stdio.h>
#include <pwd.h>
#include <errno.h>
#include <stdlib.h>
#include <signal.h>
#include "rmtl_err.h"

/*  �����ѹ�  94.05.24  start  */
/*efine A_DDLINE           4           --  �ɲ�DDʸ�Կ�                      */
/* ------------------------------------------------------------------------- */
#define A_DDLINE           5           /*  �ɲ�DDʸ�Կ�                      */
/*  �����ѹ�  94.05.24  ended  */
#define STC_PORT (u_short)1994         /*  ��ѥ���ֵ�ư�����ݡ����ֹ�      */
#define PORT     (u_short)0            /*  ���ݡ����ֹ�����������          */
#define BUFFSIZE          82           /*  �ե����륢���������Хåե�Ĺ      */
#define SENDSIZE        1000           /*  �����Хåե�Ĺ                    */
#define DATALEN          950           /*  JCL �����������å�Ĺ              */
#define ASYN               0           /*  ��Ʊ���⡼��                      */
#define SYNC               1           /*  Ʊ���⡼��                        */
#define ACCEPT             0           /*  connect����                       */
#define JCLcnf             1           /*  JCL ���Ͻ�������λ��̼���        */
#define JOBind             2           /*  ��ѥ����̾����                  */
#define RCVLEN           512           /*  �����Хåե�Ĺ                    */
/*  �����ѹ�  94.05.24  start  */
/*efine AD_DDN             3           --  ��ά���ɲ�DDʸ��                  */
/* ------------------------------------------------------------------------- */
#define AD_DDN             4           /*  ��ά���ɲ�DDʸ��                  */
/*  �����ѹ�  94.05.24  ended  */

#define OPRLEN        1024*2           /*  ���ޥ�ɥ��ڥ��ɥ����å�Ĺ      */

#define ALCTIME          180           /*  ALLOC���Թ礻���֡��á�           */
/*  �����ѹ�  94.06.18  start  */
/*efine TIME              60           --  ALLOC�ʳ����Թ礻����             */
/* ------------------------------------------------------------------------- */
#define TIME             180           /*  ALLOC�ʳ����Թ礻����             */
/*  �����ѹ�  94.06.18  ended  */

#define ALLOC              0
#define OPEN               1
#define READ               2
#define WRITE              3
#define REWIND             4
#define BACKSPACE          5
#define ENDFILE            6
#define CLOSE              7
#define FREE               8

/* ************************************************************************* */
/*   �����ѿ�����������饤�֥�궦�̰��                                    */
/* ************************************************************************* */
static char     EXT_svip[16];               /*  �����Хۥ���(MSP)IP���ɥ쥹  */
static char     EXT_svnm[33];               /*  �����Хۥ���(MSP)̾          */
static u_short  EXT_port    ;               /*  ���ݡ����ֹ�                 */
static int      EXT_sock    ;               /*  ��ѥ���֤Ȥ�socket         */
static char     EXT_jobn[9] ;               /*  ��ѥ����̾                 */
static int      EXT_csok    ;               /*  ��ѥ���֤Ȥ�connect socket */
static fd_set   EXT_mask    ;               /*  ��Ʊ���̿��ѥޥ���           */
static char     EXT_ownip[16];              /*  WS��IP���ɥ쥹               */

/* ************************************************************************* */
/*   ���̰��MTLINIT�ץ����Τ�)                                            */
/* ************************************************************************* */
static char         EXT_JCL[] = ".rmtljcl"; /*  ��ѥ��������ե�����̾     */
struct sockaddr_in  EXT_client;             /*  sockaddr_in��¤��            */
struct def  {                               /*  ��ά���ɲ�DDʸ��¤�����     */
       char         *srch_dd;               /*  1.������DD̾                 */
       char         *add_ddn;               /*  2.�ɲ�DDʸ                   */
       int          flag;                   /*  3.���JOB���FILE��̵ͭFLAG  */
};

                                           /*  ��ά���ɲ�DDʸ��¤�ν������  */
static struct def   EXT_DDN[AD_DDN] = {
       "//FT06F001 ", "//FT06F001  DD  SYSOUT=*\n", 0,
       "//CMDIN "   , "//CMDIN     DD  DUMMY\n"   , 0,
       "//SYSPRINT ", "//SYSPRINT  DD  SYSOUT=*\n", 0,
/*  �����ѹ�  94.05.24  start  */
       "//STDERR "  , "//STDERR    DD  SYSOUT=*\n", 0
/*  �����ѹ�  94.05.24  ended  */
};

int      EXT_WAIT;                          /*  �Ԥ�����                     */
char     EXT_RELAY[512];                    /*  ������ѥ��������ե�����̾ */

extern void mtl_sig();                      /*  �����ʥ����������           */
/* ************************************************************************* */
/*   ���̰��MTLINIT�ץ����ʳ�)                                            */
/* ************************************************************************* */
static int      EXT_status[100];            /*  FILE-ACCESS����TABLE         */
static char     EXT_sdbf[128];
static char     EXT_rvbf[128];

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLINT_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ��ѥ���ֳ����׵����                                               */
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlint_( char *hostname, int *time, char *jobname, int *rc )     */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      hostname    char *      in           �ۥ���̾orIP���ɥ쥹            */
/*      time        int  *      in           �������͡���ñ�̡ˡ���          */
/*      jobname     char *      out          ��ѥ����̾                    */
/*      rc          int  *      out          ����������                      */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlint_( hostname, time, jobname, rc )

  char             *hostname;          /*  �����Хۥ���̾                    */
  int              *time;              /*  WAIT��������                      */
  char             *jobname;           /*  ��ѥ����̾                      */
  int              *rc;                /*  ����������                        */

{
  int               i;                 /*  ��Ȱ�                            */
  char             *wp;                /*  ��Ȱ�                            */
  int               len;

/* ************************************************************************* */
/*  �����ѿ���������                                                       */
/* ************************************************************************* */
  memset( EXT_svip, NULL, 16);
  memset( EXT_svnm, NULL, 33);
  memset( EXT_jobn, NULL, 9 );
  EXT_port = (u_short)0;
  EXT_sock = 0;
  EXT_csok = 0;
  for ( i=0; i<100; i++ ) {
    EXT_status[i] = 0;
  }
/* ************************************************************************* */
/*  �������������                                                           */
/* ************************************************************************* */
#ifdef  SNAP
  printf( "# mtlint_ start #\n" );
  printf( "mtlint_/hostname:%s#\n", hostname );
  printf( "mtlint_/time    :%d#\n", *time    );
#endif
/*  SIGPIPE�����ʥ��̵�뤹�롣                                              */
/*  ��꤬socket��CLOSE�������send, write��¹Ԥ���ȡ�                     */
/*  �ץ�������λ���Ƥ��ޤ����ʲ���������뤳�Ȥˤ������ǽ               */
/*  send, write�ؿ���-1���֤���                                              */
  signal( SIGPIPE, SIG_IGN );

/*  SIGINT(Cntl-D)�ڤ�SIGTERM(kill���ޥ��)�����ʥ���ջ���������            */
  signal( SIGINT, mtl_sig );
  signal( SIGTERM, mtl_sig );

/*  �ۥ���̾Ĺ�������å�  */
  len = strlen( hostname );
  for ( i = len - 1; i >= 0; i-- ) {
    if ( hostname[i] != ' ' )  break;
  }
  if ( i == -1 || i > 32 ) {
    *rc = ERSVNLNG;
    return;
  }
/*  �ۥ���̾�����ѿ��������  */
  memcpy( EXT_svnm, hostname, i + 1 );

#ifdef  SNAP
  printf( "mtlint_/EXT_svnm:%s#\n", EXT_svnm );
#endif
/*  ������������  */
  if ( *time < 10 )
    EXT_WAIT = 10;
  else
    EXT_WAIT = *time;

/*  ��ѥ����JCL ����ե���������å�  */
  if ( (*rc = rmtljcl_chk( )) != NULL )
    return;
    
/*  ��ѥ����JCL ž��  */
  if ( (*rc = JCL_trans( )) != NULL )
    return;
    
/*  ��ѥ����JCL ž�����������ｪλ������硢��ѥ����̾����  */
  if ( (*rc = jobname_recv( )) == NULL )
    strcpy( jobname, EXT_jobn );
  
  return;
}
/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLATR_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ���ȥ�ӥ塼���׵����                            (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlatr_( char *oper, int *rc, int *rc1, int *rc2 )               */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      oper        char *      in           ATTRIB���ޥ�ɥ��ڥ���        */
/*      rc          int  *      out          ����������          ��          */
/*      rc1         int  *      out          IPFCMD�¹Ի�RC1                 */
/*      rc2         int  *      out          IPFCMD�¹Ի�RC2                 */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlatr_(oper, rc, rc1, rc2)
  char *oper;
  int *rc, *rc1, *rc2;
{
  int i;

/* ���ޥ�ɥ��ڥ��ɥ����å� */

  if ( strlen(oper) == 0 ) {
    *rc = EROPERAND;
    return;
  }
  for ( i = strlen(oper) - 1; i >= 0; i-- ) {
    if ( oper[i] != ' ' )  break;
  }
  if( i == -1 || i > OPRLEN - 2 ) {
    *rc = EROPERAND;
    return;
  }
/*  ATTRIB�ؼ�������  */
  if ( (*rc=SND_data( EXT_sock, "T", 1 )) !=0 )  goto error;
/*  ATTRIB���ޥ�ɥ��ڥ�������  */
  if ( (*rc=SND_data( EXT_sock, oper, i + 1 )) !=0 )  goto error;
/*  ��������  */
  if ( (*rc=SND_data(EXT_sock, "\n", 1)) != 0 ) goto error;
/*  IPFCMD��̼���  */
  if ( (*rc=RCV_rcipf(EXT_sock, 60, rc1, rc2)) != 0 ) {
    if ( *rc!=ERIPFERR ) goto error;
  }
  return;

error:;
  close(EXT_sock);
  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLALC_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ���������׵����                                (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlalc_( int *u, char *oper, int *rc, int *rc1, int *rc2 )       */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      u           int  *      in           ���ֻ����ֹ�        ��          */
/*      oper        char *      in           ALLOC���ޥ�ɥ��ڥ���         */
/*      rc          int  *      out          ����������          ��          */
/*      rc1         int  *      out          IPFCMD�¹Ի�RC1                 */
/*      rc2         int  *      out          IPFCMD�¹Ի�RC2                 */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlalc_(u, oper, rc, rc1, rc2)
  int *u;
  char *oper;
  int *rc, *rc1, *rc2;
{
  int i, flg, cnt;
  char cnv_buff[3];

#ifdef SNAP
  printf( "* mtlalc_ start *\n" );
  printf( "unit:%d\n", *u );
  printf( "oper:%s#\n", oper );
#endif 

/* ���ֻ����ֹ��ϰϥ����å� */
  if ( *u<0 || *u==6 || *u>99 ) {
    *rc = ERUNITNO;
    return; 
  }

/* ���ޥ�ɥ��ڥ��ɥ����å�  */
  if ( strlen(oper) == 0 ) {
    *rc = EROPERAND;
    return;
  }
  for ( i = strlen(oper) - 1; i >= 0; i-- ) {
    if ( oper[i] != ' ' )  break;
  }
  if ( i == -1 || i > OPRLEN - 14 ) {
    *rc = EROPERAND;
    return;
  }
/*  �ե����륢������status�����å�  */
  if( (*rc=FLE_status( 0, u, ALLOC )) !=0 ) return;

/*  ALLOC�ؼ�������  */
  if ( (*rc=SND_data( EXT_sock, "A", 1 )) != 0 ) goto error;

/*  ALLOC���ޥ�ɥ��ڥ�������  */
  if ( (*rc=SND_data( EXT_sock, oper, i + 1 )) !=0 ) goto error;

/*  ���ֻ����ֹ�����������DD̾"F(FTxxF001)"����            */
/*  ALLOC���ޥ�ɤˤ����ƺǽ����ꤵ�줿DD̾��ͭ���Ȥʤ뤿��  */
  sprintf( EXT_sdbf, " F(FT%.2dF001)\n", *u );
  if ( (*rc=SND_data( EXT_sock,
                      EXT_sdbf,
                      strlen(EXT_sdbf) )) !=0 ) goto error;
                      
/*  IPFCMD��̼���  */
  if ( (*rc=RCV_rcipf( EXT_sock, ALCTIME, rc1, rc2 )) !=0 ) {
    if ( *rc == ERIPFERR )  return;
    else goto error;
  }
  
/*  �ե����륢������status����  */
  if ( *rc == 0 )  FLE_status( 1, u, ALLOC );

  return;

error:;
  close( EXT_sock );
  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLFRE_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �ե꡼�׵����                                    (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlfre_( int *u, char *oper, int *rc, int *rc1, int *rc2 )       */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      u           int  *      in           ���ֻ����ֹ�        ��          */
/*      oper        char *      in           FREE���ޥ�ɥ��ڥ���          */
/*      rc          int  *      out          ����������          ��          */
/*      rc1         int  *      out          IPFCMD�¹Ի�RC1                 */
/*      rc2         int  *      out          IPFCMD�¹Ի�RC2                 */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlfre_(u, oper, rc, rc1, rc2)
  int *u;
  char *oper;
  int *rc, *rc1, *rc2;
{
  int i, flg, cnt;
  char cnv_buff[3];

/*  ���ֻ����ֹ��ϰϥ����å�  */
  if ( *u<-1 || *u==6 || *u>99 ) {
    *rc = ERUNITNO;
    return; 
  }

/*  ���ޥ�ɥ��ڥ��ɥ����å�  */
  for ( i = strlen(oper) - 1; i >= 0; i-- ) {
    if ( oper[i] != ' ' )  break;
  }
  if ( i > OPRLEN - 13 ) {
    *rc = EROPERAND;
    return;
  }
/*  �ե����륢������status�����å�  */
  if ( *u != -1 ) {
    if( (*rc=FLE_status( 0, u, FREE )) !=0 )  return;
  }
/*  FREE�ؼ�������  */
  if( (*rc=SND_data( EXT_sock, "F", 1 )) !=0 )  goto error;
/*  FREE���ޥ�ɥ��ڥ�������  */
  if ( i > -1 ) {
   if ( (*rc=SND_data( EXT_sock, oper, i + 1 )) !=0 )  goto error;
  }

/*  ���ֻ����ֹ�����������DD̾"F(FTxxF001)"����            */
/*  FREE���ޥ�ɤˤ����ƺǽ����ꤵ�줿DD̾��ͭ���Ȥʤ뤿��  */
  if ( *u != -1 )
    sprintf( EXT_sdbf, " F(FT%.2dF001)\n", *u );
  else
    sprintf( EXT_sdbf, "\n" );
  if ( (*rc=SND_data( EXT_sock, EXT_sdbf, strlen(EXT_sdbf) )) !=0 )
    goto error;
    
/*  IPFCMD��̼���  */
  if ( (*rc=RCV_rcipf( EXT_sock, TIME, rc1, rc2 )) !=0 ) {
    if ( *rc == ERIPFERR )  return;
    else goto error;
  }

/*  �ե����륢������status����  */
  if ( *rc == 0 && *u != -1 ) 
    FLE_status( 1, u, FREE );

  return;

error:;
  close( EXT_sock );
  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLOPN_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �����ץ��׵����                                  (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlopn_( int *u, char *opmode, int *rc )                         */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      u           int  *      in           ���ֻ����ֹ�        ��          */
/*      opmode      char *      in           �����ץ�⡼��                  */
/*                                           "READ" : �ɤ߹���               */
/*                                           "WRITE": �񤭹���               */
/*                                           "BOTH" : �ɤ߽�               */
/*      rc          int  *      out          ����������          ��          */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlopn_(u, opmode, rc)
  int *u;
  char *opmode;
  int *rc;
{
  int   mode;
  int   i;

/*  ���ֻ����ֹ��ϰϥ����å� */
  if ( *u<0 || *u==6 || *u>99 ) {
    *rc = ERUNITNO;
    return; 
  }
 
#ifdef SNAP
  printf("opmode:%s#\n", opmode );
#endif

/*  �����ץ�⡼�ɥ����å�  */
  if ( strlen( opmode ) < 4 ) {
    *rc = ERMODE;
    return;
  } 
  
  for ( i = strlen( opmode ) - 1; i >= 0; i-- ) {
    if ( memcmp( &opmode[i], " ", 1 ) != NULL ) { 
      i++;
      break;
    }
  }
  if ( i < 0 ) {
    *rc = ERMODE;
    return;
  }

  if ( (i==4) && (memcmp( opmode, "READ", 4 ) == NULL) )        mode = 0;
  else if ( (i==5) && (memcmp( opmode, "WRITE", 5 ) == NULL) )  mode = 1;
  else if ( (i==4) && (memcmp( opmode, "BOTH", 4 ) == NULL) )   mode = 2;
  else {
    *rc = ERMODE;
    return;
  }
/*  �ե����륢������status�����å�  */
  if ( (*rc = FLE_status( 0, u, OPEN )) != 0 ) return;
/*  OPEN�׵�����  */
  sprintf( EXT_sdbf, "O%d\n%d\n", *u, mode );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) !=0 ) goto error;
/*  OPEN������̼���  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) !=0 ) {
/*  �����ѹ�  94.06.18   start  */
/*  if ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) goto error;      */
/* --------------------------------------------------------------- */
    if ( ((*rc >= ERTRANS) && (*rc <= ERTRNSCLS)) || 
         ( *rc == ERTIMEOUT ) ) goto error;
/*  �����ѹ�  94.06.18   ended  */
    else return;
  }

/*  �ե����륢������status����  */
  if ( *rc == 0 )  FLE_status( 1, u, OPEN );

  return;

error:;
  close( EXT_sock );
  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLCLS_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �������׵����                                  (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlcls_( int *u, int *rc )                                       */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      u           int  *      in           ���ֻ����ֹ�        ��          */
/*      rc          int  *      out          ����������          ��          */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlcls_(u, rc)
  int *u;
  int *rc;
{
  int mode;

/*  ���ֻ����ֹ��ϰϥ����å� */
  if ( *u < 0 || *u == 6 || *u > 99 ) {
    *rc = ERUNITNO;
    return; 
  }

/*  �ե����륢������status�����å�  */
  if ( (*rc = FLE_status( 0, u, CLOSE )) != 0 )
    return;
/*  CLOSE�׵�����  */
  sprintf( EXT_sdbf, "C%d\n", *u );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto error;
/*  CLOSE������̼���  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) != 0 ) {
/*  �����ѹ�  94.06.18  start   */
/*  if ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) )              */
/*    goto error;                                              */
/* ----------------------------------------------------------- */
    if ( ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) ||
         ( *rc == ERTIMEOUT ) )
      goto error;
/*  �����ѹ�  94.06.18  ended   */
    return;
  }

/*  �ե����륢������status����  */
  if ( *rc == 0 )
    FLE_status( 1, u, CLOSE );

  return;

error:;
  close( EXT_sock );
  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLRED_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �꡼���׵����                                    (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlred_( int *u, char *buf, int *len, int *rc )                  */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      u           int  *      in           ���ֻ����ֹ�        ��          */
/*      buf         char *      out          �ɤ߹����ΰ�                    */
/*      len         int  *      i/o          �ɤ߹����ΰ�Ĺ(in)              */
/*                                           1FORTRAN��ϿĹ(out)             */
/*      rc          int  *      out          ����������          ��          */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlred_(u, buf, len, rc)
  int *u, *len, *rc;
  char *buf;
{
/*  ���ֻ����ֹ��ϰϥ����å� */
  if ( *u < 0 || *u == 6 || *u > 99 ) {
    *rc = ERUNITNO;
    return; 
  }

/*  �ɤ߹����ΰ�Ĺ�����å� */
  if ( *len <= 0 ) {
    *rc = ERLENGTH;
    return;
  }

/* �ɤ߹����ΰ�ν���� */
  memset(buf, '\0', *len);

/*  �ե����륢������status�����å�  */
  if ( (*rc = FLE_status( 0, u, READ )) != 0 )
    return;
/*  READ�׵�����  */
  sprintf(EXT_sdbf, "R%d\n%d\n", *u, *len);
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto error;
/*  READ��̤����READ-DATA����  */
  if ( (*rc = RCV_rcread( EXT_sock, TIME, buf, len )) != 0 ) {
    if ( *rc == ERBUFSIZE )
      return;
    else {
/*  �����ѹ�  94.06.18   start   */
/*    if ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) )           */
/* ---------------------------------------------------------- */
      if ( ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) ||
           ( *rc == ERTIMEOUT ) )
/*  �����ѹ�  94.06.18   ended   */
        goto error;
      else
        return;
    }
  }

/*  �ե����륢������status����  */
  if ( *rc == 0 )  FLE_status( 1, u, READ );
  
  return;

error:;
  close( EXT_sock );
  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLWRT_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �饤���׵����                                    (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlwrt_( int *u, char *buf, int *len, int *rc )                  */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      u           int  *      in           ���ֻ����ֹ�        ��          */
/*      buf         char *      in           �񤭹��ߥǡ�����Ǽ�ΰ�          */
/*      len         int  *      in           �񤭹��ߥǡ���Ĺ                */
/*      rc          int  *      out          ����������          ��          */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlwrt_(u, buf, len, rc)
  int *u, *len, *rc;
  char *buf;
{
/*  ���ֻ����ֹ��ϰϥ����å� */
  if ( *u < 0 || *u == 6 || *u > 99 ) {
    *rc = ERUNITNO;
    return; 
  }

/*  �񤭹����ΰ�Ĺ�����å� */
  if ( *len <= 0 ) {
    *rc = ERLENGTH;
    return;
  }

/*  �ե����륢������status�����å�  */
  if ( (*rc = FLE_status( 0, u, WRITE )) != 0 )
    return;
/*  WRITE�׵�����  */
  sprintf( EXT_sdbf, "W%d\n%d\n", *u, *len );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto error;
/*  WRITE������̼���  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) != 0 )
    goto error;
/*  �񤭹����ΰ�(WRITE-DATA)����  */
  if ( (*rc = SND_data( EXT_sock, buf, *len )) != 0 )
    goto error;
/*  WRITE��̼���  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) != 0 ) {
/*  �����ѹ�  94.06.18   start   */
/*  if ( (*rc>=ERTRANS) && (*rc<=ERTRNSCLS) )     */
/* ---------------------------------------------- */
    if ( ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) ||
         ( *rc == ERTIMEOUT ) )
/*  �����ѹ�  94.06.18   ended   */
      goto error;
    else
      return;
  }
  
/*  �ե����륢������status����  */
  if ( *rc == 0 )  FLE_status( 1, u, WRITE );

  return;

error:;
  close( EXT_sock );
  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLCTL_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ����ȥ����׵����                              (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlctl_( int *u, char *action, int *rc )                         */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      u           int  *      in           ���ֻ����ֹ�        ��          */
/*      action      char *      in           ����ȥ���ؼ���              */
/*                                           "REWIND"    : ��磻���        */
/*                                           "BACKSPACE" : �Хå����ڡ���    */
/*                                           "ENDFILE"   : ����ɥե�����    */
/*      rc          int  *      out          ����������          ��          */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlctl_(u, action, rc)
  int *u;
  char *action;
  int *rc;
{
  int mode;
  int i;

/*  ���ֻ����ֹ��ϰϥ����å� */
  if ( *u < 0 || *u == 6 || *u > 99 ) {
    *rc = ERUNITNO;
    return; 
  }
/*  ����ȥ���ؼ��ҥ����å�  */
  for ( i = strlen( action ) - 1; i >= 0; i-- ) {
    if ( memcmp( &action[i], " ", 1 ) != NULL ) {
      i++;
      break;
    }
  }
  if ( i < 6 ) {
    *rc = ERMODE;
    return;
  }

  if ( (i==6) && (memcmp( action, "REWIND", 6 ) == NULL) )          mode=0;
  else if ( (i==9) && (memcmp( action, "BACKSPACE", 9 ) == NULL) )  mode=1;
  else if ( (i==7) && (memcmp( action, "ENDFILE", 7 ) == NULL) )    mode=2;
  else {
    *rc = ERMODE;
    return;
  }

/*  �ե����륢������status�����å�  */
  if ( (*rc = FLE_status( 0, u, mode+4 )) != 0 )
    return;
/*  ����ȥ����׵�����  */
  sprintf( EXT_sdbf, "I%d\n%d\n", *u, mode );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto error;
/*  ����ȥ����̼���  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) != 0 ) {
/*  �����ѹ�  94.06.18   start   */
/*  if ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) )       */
/* ---------------------------------------------------- */
    if ( ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) ||
         ( *rc == ERTIMEOUT ) )
/*  �����ѹ�  94.06.18   ended   */
      goto error;
    else
      return;
  }

/*  �ե����륢������status����  */
  if ( *rc == 0 )
    FLE_status( 1, u, mode+4 );

  return;

error:;
  close( EXT_sock );
  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : MTLEND_                                        */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ��ѥ���ֽ�λ�׵����                            (FORTRAN interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      void mtlend_( int *rc )                                               */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      rc          int  *      out          ����������          ��          */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlend_(rc)
  int *rc;
{
/*  ��ѥ���ֽ�λ�׵�����  */
  sprintf( EXT_sdbf, "E\n" );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto ende;
/*  �����Ф����socket�������Թ礻  */
  *rc = RCV_rc( EXT_sock, TIME );

  switch ( *rc ) {
    case NORMAL    :  *rc=ERTRNSPRO;
                      break;
    case ERTRNSCLS :  *rc=NORMAL;
                      break;
    case ERTIMEOUT :  goto ende;
    default        :  break;
  }

ende:;
  close( EXT_sock );

  return;
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �����ʥ����������                                                   */
/*                                                                           */
/*  ��  ��                                                                   */
/*      void  mtl_sig( )                                                     */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �����                                                                   */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
void  mtl_sig()
{
/* ************************************************************************* */
/*   Cntl-C  �ڤ�  kill���ޥ�ɼ���������                                    */
/* ************************************************************************* */
  exit( 0 );
}

/* ************************************************************************* */
/*  ���ʥ⥸�塼��  Function list                                            */
/*                                                                           */
/*  used by mtlint_                                                           */
/*                                                                           */
/*  - JCL_trans()                                                            */
/*    ��ѥ����ž������                                                     */
/*  - rmtljcl_chk()                                                          */
/*    ��ѥ��������ե���������å�                                         */
/*  - ip_port_get()                                                          */
/*    ��IP���ɥ쥹w��s�ݡ����ֹ����                                          */
/*  - JCL_send()                                                             */
/*    ��ѥ����JCLž��                                                      */
/*  - JCL_data()                                                             */
/*    ��ѥ��������ե�������JCL������ɲ�DDʸž��                          */
/*  - jobname_recv()                                                         */
/*    ��ѥ����̾�򥵡��Хۥ���(MSP)������                                */
/*  - data_rcv_anl()                                                         */
/*    �����Хۥ���(MSP)����������ǡ�������w����                            ��
��  ���������                                                           ��
��    ��ѥ����JCL��λ�ԥ����å�                                            */
/*                                                                           */
/* ************************************************************************* */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ��ѥ��������ե���������å�                                       */
/*                                                                           */
/*  ��  ��                                                                   */
/*      int rmtljcl_chk( )                                                   */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  rmtljcl_chk( )

{
  FILE             *JCLFILE;           /*  ��ѥ��������ե�����ݥ���    */
  struct stat       filemode;          /*  �ե�����⡼�ɹ�¤��              */
  u_short           mode;              /*  �ե�����⡼��                    */
  char              buff[BUFFSIZE];    /*  �ե�����READ�Хåե�              */
  char             *file;              /*  ��ѥ��������ե�����̾�ݥ���  */

/* ************************************************************************* */
/*  ��ѥ��������ե���������å�                                           */
/* ************************************************************************* */
#ifdef  SNAP
  printf( "# rmtljcl_chk start #\n" );
#endif
/*  �Ķ��ѿ����������Ƥ����硢����ե�����̾�����  */
/*  �������Ƥ��ʤ���硢�����ȥǥ��쥯�ȥ��۲���".rmtljcl"���� */
  if ( (file=getenv( "RMTLJCL" )) != NULL )
    strcpy( EXT_RELAY, file );
  else
    strcpy( EXT_RELAY, EXT_JCL );

/*  �Ķ��ѿ���null�������Ƥ����硢
���������ȥǥ��쥯�ȥ��۲���".rmtljcl"����  */
  if ( strlen( EXT_RELAY ) == 0 )
    strcpy( EXT_RELAY, EXT_JCL );

#ifdef  SNAP
  printf( "rmtljcl_chk/EXT_RELAY:%s#\n", EXT_RELAY );
#endif

/*  ����ե�����̵ͭ�����å�  */
  if ( access( EXT_RELAY, 0 ) != NULL )
    return( ERFLNOT );

/*  �ե�����⡼�ɼ���  */
  if ( stat( EXT_RELAY, &filemode ) != NULL )
    return( ERFLACS );

/*  �ե����륢�������������å���¾�Ԥ���Υ������������ʤ�����  */
  mode = filemode.st_mode;
  mode = mode & S_IRWXO;
  if ( mode != NULL )
    return( ERFLACS );

  mode = filemode.st_mode;
  mode = mode & S_IRWXG;
  if ( mode != NULL )
    return( ERFLACS );

/*  �ե�����쥳����Ĺ�����å�  */
  if ( (JCLFILE=fopen( EXT_RELAY, "r" )) == NULL )
    return( ERFLUNEX );
    
  while ( fgets( buff, BUFFSIZE, JCLFILE ) != NULL ) {
    if ( strchr( buff, '\n' ) == NULL ) {
      fclose( JCLFILE );
      return( ERFLLRL );
    }
  }
  if ( ferror( JCLFILE ) ) {
    fclose( JCLFILE );
    return( ERFLUNEX );
  }
  fclose ( JCLFILE );
  
#ifdef  SNAP
  printf( "# rmtljcl_chk ENDED #\n" );
#endif

  return( NORMAL );
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ��ѥ����JCL ž��                                                   */
/*                                                                           */
/*  ��  ��                                                                   */
/*      int JCL_trans( )                                                     */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  JCL_trans( )

{
  struct hostent     *MSP;             /*  hostent��¤��                     */
  struct sockaddr_in  server;          /*  sockaddr_in��¤��                 */
  int                 i, j;            /*  ��Ȱ�                            */
  char                wk_host[40];     /*  �����Хۥ���̾��Ȱ�              */
  char               *wp, *fp;         /*  IP���ɥ쥹�����å�����ȥݥ���  */
  unsigned int        u_add;
  unsigned char       uchr[4];
  int                 s_sok;           /*  �����Хۥ���(MSP)�Ȥ�socket       */
 
/* ************************************************************************* */
/*  ��ѥ����JCL ž��                                                       */
/* ************************************************************************* */
#ifdef  SNAP
  printf( "# JCL_trans start #\n" );
  printf( "JCL_trans/EXT_svnm:%s#\n", EXT_svnm );
#endif
/*  �ۥ���̾��IP���ɥ쥹���꤫����  */
  wp = EXT_svnm;
  for ( i=0; i<3; i++,wp++ ) {
    if ( (wp=strchr( wp, '.' )) == NULL )
      break;
  }
/*  IP���ɥ쥹�Ȥ���"."�ο���������  */
  if ( i == 3 ) {
/*  IP���ɥ쥹���������å�  */
    sprintf( wk_host, "%s.", EXT_svnm );
    j = 0;
    wp = wk_host;
    while ( (fp=strchr(wp,'.')) != NULL ) {
      if ( (fp-wp<1) || (fp-wp>3) )
        goto hostnm;
      memset( fp, NULL, 1 );
      for ( i=0; i<strlen(wp); i++ ) {
        if ( isdigit( wp[i] ) == 0 ) goto hostnm;
      }
      if ( (atoi(wp) < 0) || (atoi(wp) > 255) )
        return( ERSVNIP );
      if ( j < 4 ) {
        u_add = (u_short)atoi(wp);
        uchr[j] = (u_char)u_add;
      }
      wp = fp + 1;
      j++;
    }

    strcpy( EXT_svip, EXT_svnm );
    goto conproc;
  }

hostnm:;
/*  �ۥ���̾���������  */
  if ( (MSP=gethostbyname(EXT_svnm)) == NULL )
    return( ERSVNNOT );

  sprintf( EXT_svip, "%u.%u.%u.%u",
           (u_char) MSP->h_addr[0],
           (u_char) MSP->h_addr[1],
           (u_char) MSP->h_addr[2],
           (u_char) MSP->h_addr[3] );

  for ( i=0; i<4; i++ ) {
    uchr[i] = (u_char) MSP->h_addr[i];
  }

conproc:;
#ifdef  SNAP
  printf( "JCL_trans/EXT_svip:%s#\n", EXT_svip );
#endif

/*  �����Хۥ���(MSP)�Ȥ�connect��Ω  */
  memset( &server.sin_family, NULL, sizeof(struct sockaddr_in) );
  server.sin_family = AF_INET;
  server.sin_port   = STC_PORT;
  bcopy( uchr, (char *)&server.sin_addr, 4 );

  if ( (s_sok=socket( AF_INET, SOCK_STREAM, 0 )) < 0 )
    return( ERTRNSSOK );

#ifdef  SNAP
  printf( "JCL_trans/socket create s_sok:%d#\n", s_sok );
#endif

  if ( connect( s_sok, &server, sizeof(struct sockaddr_in) ) < 0 ) {
#ifdef  SNAP
  printf( "JCL_trans/connect error no   :%d#\n", errno );
#endif
    close( s_sok );
    return( ERTRNSCON );
  }

#ifdef  SNAP
  printf( "JCL_trans/connection s_sok:%d#\n", s_sok );
#endif

/*  ��IP���ɥ쥹w��s�ݡ����ֹ����  */
  if ( (i=ip_port_get( s_sok, server )) != NULL ) {
    close( s_sok );
    return( i );
  }

  FD_ZERO( &EXT_mask );
  FD_SET( s_sok, &EXT_mask );

/*  ��ѥ����JCL ž��  */
  i=JCL_send( s_sok );
  close( s_sok );

#ifdef  SNAP
  printf( "# JCL_trans ended #\n" );
#endif

  return( i );
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ��IP���ɥ쥹w��s�ݡ����ֹ����                                        */
/*                                                                           */
/*  ��  ��                                                                   */
/*      int ip_port_get( s_sok )                                             */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      s_sok       int         in           ��ѥ����JCL ž����socket      */
/*      server      struct      in           ��ѥ����JCL ž����sockaddr_in */
/*                  sockaddr_in                                              */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  ip_port_get( s_sok, server )

  int                 s_sok;
  struct sockaddr_in  server;

{
  int                 len;             /*  sockaddr_in��¤��Ĺ               */

/* ************************************************************************* */
/*  ��IP���ɥ쥹w��s�ݡ����ֹ����                                            */
/* ************************************************************************* */
#ifdef  SNAP
  printf( "# ip_port_get start #\n" );
#endif

/*  ��IP���ɥ쥹����  */
  len = (int) sizeof(server);
  if ( getsockname( s_sok, &server, &len ) < 0 )
    return( ERWSIPGET );

  sprintf( EXT_ownip,"%u.%u.%u.%u",
          (u_char)server.sin_addr.S_un.S_un_b.s_b1,
          (u_char)server.sin_addr.S_un.S_un_b.s_b2,
          (u_char)server.sin_addr.S_un.S_un_b.s_b3,
          (u_char)server.sin_addr.S_un.S_un_b.s_b4 );

#ifdef  SNAP
  printf( "ip_port_get/own IP address:%s#\n", EXT_ownip );
#endif

/*  ���ݡ����ֹ椪�����ѥ����̾�Ȥ�connect�ǥ�������ץ�����  */
  memset( (char *)&EXT_client.sin_family, NULL, sizeof(struct sockaddr_in) );
  EXT_client.sin_family = AF_INET;
  EXT_client.sin_port   = PORT;
  bcopy( (char *)&server.sin_addr, (char *)&EXT_client.sin_addr, 4 );

  if ( (EXT_csok=socket( AF_INET, SOCK_STREAM, 0 )) < 0 )
    return( ERPORTSOK );

#ifdef  SNAP
  printf( "ip_port_get/DATA connect socket EXT_csok:%d#\n", EXT_csok );
#endif

  if ( bind( EXT_csok, &EXT_client, sizeof(EXT_client) ) != 0 ) {
    close( EXT_csok );
    return( ERPORTBIND );
  }

#ifdef  SNAP
  printf( "ip_port_get/bind successful EXT_csok:%d#\n", EXT_csok );
#endif

  len = (int)sizeof(EXT_client);
  if ( getsockname( EXT_csok, &EXT_client, &len ) < 0 ) {
    close( EXT_csok );
    return( ERPORTGET );
  }

  EXT_port = EXT_client.sin_port;

#ifdef  SNAP
  printf( "ip_port_get/DATA PORT No. EXT_port:%hd#\n", EXT_port );
  printf( " # ip_port_get ended #\n" );
#endif

  return( NORMAL );
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      JCL ž������                                                         */
/*                                                                           */
/*  ��  ��                                                                   */
/*      int JCL_send( s_sok )                                                */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      s_sok       int         in           ��ѥ����JCL ž����socket      */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  JCL_send( s_sok )

  int   s_sok;

{
  FILE             *JCLFILE;           /*  ��ѥ��������ե�����ݥ���    */
  struct passwd    *uid;               /*  �桼��ID��¤��                    */
  char              buff[BUFFSIZE];    /*  �ե�����READ�Хåե�              */
  char              sendbuf[SENDSIZE]; /*  �����Хåե�                      */
  char              readybf[SENDSIZE]; /*  �����ѽ����Хåե�                */
  int               jcl_line;          /*  ��ѥ����JCL �Կ�                */
  int               add_line;          /*  ��ά���ɲ�DDʸ�Կ�                */
  char              w_char[80];        /*  ��Ȱ�ʸ�����                    */
  int               len;               /*  ��Ȱ�Ĺ��                        */
  int               rtcode;            /*  ����������                        */
  int               prim;              /*  �����ǡ�������                    */
  char             *dd_area;           /*  �ɲ�DDʸ������ݥ���            */
  int               i;                 /*  �����󥿺�Ȱ�                    */

/* ************************************************************************* */
/*  JCL ž������                                                             */
/* ************************************************************************* */
#ifdef  SNAP
  printf( " # JCL_send start/s_sok:%d#\n", s_sok );
#endif

/*  WS�Υ桼��ID����  */
  if ( (uid=getpwuid( getuid() )) == NULL )
    return( ERTRANS );

#ifdef  SNAP
  printf( "JCL_send/own userid:%s#\n", uid->pw_name );
#endif

/*  ��ѥ��������ե�����Կ��������  */
  if ( (JCLFILE=fopen( EXT_RELAY, "r" )) == NULL )
    return( ERTRANS );
  
  for ( i=0; i<AD_DDN; i++ ) {
    EXT_DDN[i].flag = 0;
  }
  
  jcl_line = 0;
  while( fgets( buff, BUFFSIZE, JCLFILE ) != NULL ) {
    if ( jcl_check( buff ) != 0 )
      break;
    jcl_line++;
    /*  ��ά���ɲ�DDʸ̵ͭ����  */
    for ( i=0; i<AD_DDN; i++ ) {
      if ( memcmp( buff, EXT_DDN[i].srch_dd, 
                         strlen(EXT_DDN[i].srch_dd) ) == NULL ) {
        EXT_DDN[i].flag = 1;
        break;
      }
    }
  }
  if ( ferror( JCLFILE ) ) {
    fclose( JCLFILE );
    return( ERTRANS );
  }

  fclose ( JCLFILE );

/*  �ɲ�DDʸ(DD̾:MTLIOINF�ڤӾ�ά���ɲ�DDʸ)����  */
  add_line = 0;
  for ( i=0; i<AD_DDN; i++ ) {
    if ( EXT_DDN[i].flag == 0 )  add_line++;
  }
  if ( (dd_area=(char *)malloc((add_line+A_DDLINE)*82)) == NULL ) {
    return( ERTRANS );
  }
  memset( dd_area, NULL, 1 );
  for ( i=0; i<AD_DDN; i++ ) {
    if ( EXT_DDN[i].flag == 0 )
      strcat( dd_area, EXT_DDN[i].add_ddn );
  }

  sprintf( &dd_area[strlen( dd_area )],
           "//MTLIOINF  DD  *\n%s\n%hd\n/*\n",
           EXT_ownip, EXT_port );

  jcl_line += A_DDLINE;
  jcl_line += add_line;

#ifdef  SNAP
  printf( "JCL_send/send JCLline jcl_line:%d\n", jcl_line );
  printf( "JCL_send/add DDline:\n%s", dd_area );
#endif

/*  JCL �����׵�����  */
  sprintf( readybf, "%d\n%d\n%s\n%d\n%s\n", 
                    jcl_line,
                    strlen( uid->pw_name ),
                    uid->pw_name,
                    strlen( EXT_ownip ),
                    EXT_ownip );

  sprintf( sendbuf, "J0\n%d\n%s", strlen( readybf ), readybf );

#ifdef  SNAP
  printf( "JCL_send/JCLreq:%s", sendbuf );
#endif

  if ( send( s_sok, sendbuf, strlen(sendbuf),0 ) < 0 )
    return( ERTRNSSND );

/*  JCL ���ϱ�������  */
  rtcode = data_rcv_anl( s_sok, SYNC, &prim );

#ifdef  SNAP
  printf( "JCL_send/JCLcnf recv rtcode:%d\n", rtcode );
  printf( "JCL_send/JCLcnf recv prim  :%d\n", prim   );
#endif

  switch ( rtcode ) {
    case( 0 ):                              /*  ���ｪλ       */
      {
         if ( prim != JCLcnf )
           return( ERTRANS );
         break;
      }
    case( 1 ):                              /*  -cnf ����       */
        return( ERTRNSERR );
    case( 2 ):                              /*  �ץ�ȥ��륨�顼*/
        return( ERTRNSPRO );
    case( -1 ):                             /*  �۾ｪλ         */
        return( ERTRNSRCV );
    case( -2 ):                             /*  �����ॢ����ȯ��  */
        return( ERTIMEOUT );
  }

/*  JCL �ǡ���ž��  */
  if ( (rtcode=JCL_data( s_sok, dd_area )) != NULL ) {
    free( dd_area );
    return( rtcode );
  }
  free( dd_area );

/*  JCL ��λ�׵�����  */
  strcpy( sendbuf, "J2\n0\n" );

#ifdef  SNAP
  printf( "JCL_send/JCL-ENDreq:%s", sendbuf );
#endif

  if ( send( s_sok, sendbuf, strlen(sendbuf),0 ) < 0 )
    return( ERTRNSSND );

/*  JCL ��λ��������  */
  rtcode = data_rcv_anl( s_sok, SYNC, &prim );

#ifdef  SNAP
  printf( "JCL_send/JCL-ENDcnf recv rtcode:%d\n", rtcode );
  printf( "JCL_send/JCL-ENDcnf recv prim  :%d\n", prim   );
#endif

  switch ( rtcode ) {
    case( 0 ):                              /*  ���ｪλ       */
      {
         if ( prim != JCLcnf )
           return( ERTRANS );
         break;
      }
    case( 1 ):                              /*  -cnf ����       */
        return( ERTRNSERR );
    case( 2 ):                              /*  �ץ�ȥ��륨�顼*/
        return( ERTRNSPRO );
    case( -1 ):                             /*  �۾ｪλ         */
        return( ERTRNSRCV );
    case( -2 ):                             /*  �����ॢ����ȯ��  */
        return( ERTIMEOUT );
  }

#ifdef  SNAP
  printf( "# JCL_send ended #\n" );
#endif

/*  ���ｪλ  */
  return( NORMAL );
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      JCL �ǡ���ž��                                                       */
/*                                                                           */
/*  ��  ��                                                                   */
/*      int JCL_data( s_sok, dd_area )                                       */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      s_sok       int         in           ��ѥ����JCL ž����socket      */
/*      dd_area     char *      in           �ɲ�DDʸ��Ǽ��                  */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*     -1:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  JCL_data( s_sok, dd_area )

  int    s_sok;
  char  *dd_area;

{
                                       /*  JCL �ǡ���HEADER����              */
  static char      header[]="J1\n1000\n";
  FILE             *JCLFILE;           /*  ��ѥ��������ե�����ݥ���    */
  char              buff[BUFFSIZE];    /*  �ե�����READ�Хåե�              */
  char              sendbuf[SENDSIZE]; /*  �����Хåե�                      */
  char              readybf[SENDSIZE]; /*  �����ѽ����Хåե�                */
  int               len;               /*  Ĺ���Ѻ�Ȱ�                      */
  int               send_flag;         /*  �����ե饰 0:̤���� 1:������      */
  int               rtcode;            /*  ����������                        */

/* ************************************************************************* */
/*  JCL �ǡ���ž��                                                           */
/* ************************************************************************* */

#ifdef  SNAP
  printf( "# JCL_data start #\n" );
#endif

/*  ��ѥ��������ե�����OPEN  */
  if ( (JCLFILE=fopen( EXT_RELAY, "r" )) == NULL )
    return( ERTRANS );
  
/*  ��ѥ����JCL ž��  */
  memset( readybf, NULL, SENDSIZE );
  
  len = strlen( header );
  while( fgets( buff, BUFFSIZE, JCLFILE ) != NULL ) {
#ifdef SNAP
  printf( "JCL_data/read_buff:%d#\n", strlen( buff ) );
#endif
    if ( jcl_check( buff ) != 0 )
      break;
    if ( (len + strlen( buff )) >= SENDSIZE ) {
      sprintf( sendbuf, "J1\n%d\n%s", 
                         strlen( readybf ),
                         readybf );
#ifdef  SNAP
  printf( "JCL_data/JCL-DATAreq:%s", sendbuf );
  printf( "JCL_data/JCL_DATAreq:%d#\n", strlen( sendbuf ) );
#endif
      if ( send( s_sok, sendbuf, strlen( sendbuf ),0 ) < 0 ) {
        fclose( JCLFILE );
        return( ERTRNSSND );
      }
      memset( readybf, NULL, SENDSIZE );
      len = strlen( header );
      send_flag = 1;
    }
    else {
      strcat( readybf, buff );
      len += strlen( buff );
      send_flag = 0;
    }
  }
  if ( ferror( JCLFILE ) ) {
    fclose( JCLFILE );
    return( ERTRANS );
  }

  fclose ( JCLFILE );

/*  �ɲ�DDʸž��  */
#ifdef  SNAP
  printf( "JCL_data/send_flag:%d#\n", send_flag );
#endif
  if ( send_flag == 1 ) {
    sprintf( sendbuf, "J1\n%d\n%s",
                      strlen( dd_area ),
                      dd_area );
#ifdef  SNAP
  printf( "JCL_data/JCL-DATAreq:%s", sendbuf );
  printf( "JCL_data/JCL_DATAreq:%d#\n", strlen( sendbuf ) );
#endif
    if ( send( s_sok, sendbuf, strlen( sendbuf ), 0 ) < 0 )
      return( ERTRNSSND );
  }
  else {
    if ( (len+strlen(dd_area)) <= SENDSIZE ) {
      strcat( readybf, dd_area );
      sprintf( sendbuf, "J1\n%d\n%s",
                         strlen( readybf ),
                         readybf );
#ifdef  SNAP
  printf( "JCL_data/JCL-DATAreq:%s", sendbuf );
  printf( "JCL_data/JCL_DATAreq:%d#\n", strlen( sendbuf ) );
#endif
      if ( send( s_sok, sendbuf, strlen( sendbuf ), 0 ) < 0 )
        return( ERTRNSSND );
    }
    else {
#ifdef  SNAP
  printf( "JCL_data/JCL-DATAreq:%s", sendbuf );
  printf( "JCL_data/JCL_DATAreq:%d#\n", strlen( sendbuf ) );
#endif
      if ( send( s_sok, sendbuf, strlen( sendbuf ), 0 ) < 0 )
        return( ERTRNSSND );
      sprintf( sendbuf, "J1\n%d\n%s", strlen( dd_area ), dd_area );
#ifdef  SNAP
  printf( "JCL_data/JCL-DATAreq:%s", sendbuf );
  printf( "JCL_data/JCL_DATAreq:%d#\n", strlen( sendbuf ) );
#endif
      if ( send( s_sok, sendbuf, strlen( sendbuf ), 0 ) < 0 )
        return( ERTRNSSND );
    }
  }

#ifdef  SNAP
  printf( "# JCL_data ended #\n" );
#endif

/*  ���ｪλ  */
  return( NORMAL );
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ��ѥ����̾����                                                     */
/*                                                                           */
/*  ��  ��                                                                   */
/*      int jobname_recv( )                                                  */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*      �ʤ�                                                                 */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*      4:�����ॢ����                                                       */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  jobname_recv( )

{
  int               rtcode;            /*  ����������                        */
  int               size;              /*  ��Ȱ�                            */
  int               prim;              /*                                    */
 
/* ************************************************************************* */
/*  ��ѥ����̾����                                                         */
/* ************************************************************************* */

#ifdef  SNAP
  printf( "# jobname_recv start #\n" );
#endif

/*  ��ѥ���֤����connect�Ԥ����塼����  */
  if ( listen( EXT_csok, 1 ) < 0 ) {
    close( EXT_csok );
    return( ERJOBNLSN );
  }
/*  ��ѥ���֤����connect�Թ�碌  */
  FD_ZERO( &EXT_mask );
  FD_SET( EXT_csok, &EXT_mask );
  rtcode = data_rcv_anl( EXT_csok, ASYN, &prim );

#ifdef  SNAP
  printf( "jobname_recv/connect recv rtcode:%d\n", rtcode );
  printf( "jobname_recv/connect recv prim  :%d\n", prim   );
#endif

  switch ( rtcode ) {
    case  0 : {
      size = (int)sizeof(struct sockaddr_in);
      if ( (EXT_sock=accept(EXT_csok,&EXT_client,&size)) < 0 ) {
        close( EXT_csok );
        return( ERJOBNACT );
      }
#ifdef  SNAP
  printf( "jobname_recv/accept send EXT_sock:%d\n", EXT_sock );
#endif
      break;
    }
    case  2 : {
      close( EXT_csok );
      return( ERTRNSPRO );
    }
    case -2 : {
      close( EXT_csok );
      return( ERTIMEOUT );
    }
    default : {
      close( EXT_csok );
      return( ERJOBNGET );
    }
  }   /*  end of switch  */

/*  ��ѥ���֤�����ѥ����̾����  */
  FD_ZERO( &EXT_mask );
  FD_SET( EXT_sock, &EXT_mask );
  rtcode = data_rcv_anl( EXT_sock, SYNC, &prim );

#ifdef  SNAP
  printf( "jobname_recv/jobname recv rtcode:%d\n", rtcode );
  printf( "jobname_recv/jobname recv prim  :%d\n", prim   );
#endif

  if ( rtcode != 0 || (rtcode==0 && prim!=JOBind) ) {
      close( EXT_sock );
      close( EXT_csok );
      if ( rtcode == 2 )
        return( ERTRNSPRO );
      else
        return( ERJOBNGET );
  }

#ifdef  SNAP
  printf( "# jobname_recv ended #\n" );
#endif

  return( NORMAL );
}
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �ǡ�����������                                                       */
/*                                                                           */
/*  ��  ��                                                                   */
/*      int data_rcv_anl( w_sok, mode, prim )                                */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      w_sok       int         in           �����åȥǥ�������ץ�          */
/*      mode        int         in           �����⡼��                      */
/*      prim        int  *      out          ��������                        */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*      1:-cnf����                                                           */
/*      2:�ץ�ȥ��륨�顼                                                   */
/*     -1:�۾ｪλ                                                           */
/*     -2:�����ॢ����                                                       */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  data_rcv_anl( w_sok, mode, prim )

  int               w_sok;             /*  �����åȥǥ�������ץ�            */
  int               mode;              /*  �����⡼��                        */
  int              *prim;              /*  ��������                          */

{
  fd_set            wk_mask;           /*  select�Ѻ�ȥޥ�����              */
  struct timeval    timer;             /*  select�������ѹ�¤��              */
  char              rcvbuf[RCVLEN];    /*  �����ǡ�����Ǽ��                  */
  int               rcvlen;            /*  �����ǡ���Ĺ                      */
  int               srch_sd;           /*  �����ǥ�������ץ�w��r�å�         */
  int               rtcode;            /*  ����������                        */
  char             *wp;                /*                                    */
  int               len;               /*                                    */

/* ************************************************************************* */
/*  �ǡ������������ϡ�                                                       */
/* ************************************************************************* */
/*  select�ޥ���copy  */
  wk_mask = EXT_mask;
/*  �����ǥ�������ץ����ӥå�����  */
  srch_sd = w_sok + 1;
/*  ��Ʊ���⡼�ɽ���  -  ������ֻ����Թ礻  */
  if ( mode == ASYN ) {
    timer.tv_sec = (long)EXT_WAIT;
    timer.tv_usec = 0;
    rtcode = select( srch_sd, &wk_mask, NULL, NULL, &timer );
    if ( rtcode == NULL )
      return( -2 );
    else if ( rtcode < 0 )
      return( -1 );
  }
  else {
/*  Ʊ���⡼�ɽ���  - ����ȯ���ޤ��Թ礻  */
    if ( select( srch_sd, &wk_mask, NULL, NULL, NULL ) < 0 )
      return( -1 );
  }

/*  �����ǡ�������  */
  for ( ;; ) {
    if ( FD_ISSET( w_sok, &wk_mask) ) {
      if ( w_sok == EXT_csok ) {
        *prim = ACCEPT;
        return( 0 );
      }
      else {
        memset( rcvbuf, NULL, RCVLEN );
        if ( (rcvlen = recv( w_sok, rcvbuf, RCVLEN, 0 )) <= 0 )
          return( -1 );
      }
      break;
    }
  }

/*  �����ǡ�������  */
  if ( memcmp( rcvbuf, "A0", 2 ) == NULL )
    *prim = JCLcnf;
  else if ( memcmp( rcvbuf, "J3", 2 ) == NULL ) {
    *prim = JOBind;
    if ( (wp=strchr( &rcvbuf[3],'\n' )) != NULL ) {
      memset( wp, NULL, 1 );
      len = atoi( &rcvbuf[3] );
      if ( len <= 0 || len > 8 )
        return( -1 );
      wp++;
      if ( strchr(wp,'\n') != NULL ) {
        memset( strchr(wp,'\n'), NULL, 1 );
        strcpy( EXT_jobn, wp );
      }
      else
        return( 2 );
    }
    else
      return( 2 );
  }
  else if ( (memcmp( rcvbuf, "A1", 2 ) == NULL) ||
            (memcmp( rcvbuf, "A2", 2 ) == NULL) ||
            (memcmp( rcvbuf, "A3", 2 ) == NULL) ) {
    *prim = JCLcnf;
    return( 1 );
  }
  else
    return( 2 );

/*  ���ｪλ  */
  return( 0 );
}
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ��ѥ����JCL��λ�ԥ����å�                                          */
/*                                                                           */
/*  ��  ��                                                                   */
/*      int jcl_check( jcl )                                                 */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      jcl         char *      in           JCL-READ��                      */
/*                                                                           */
/*  �����                                                                   */
/*      0:��λ�ԤǤϤʤ�                                                     */
/*     -1:��λ�ԤǤ���                                                       */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  jcl_check( jcl )

  char             *jcl;               /*  JCL-READ��                        */

{
  int               i;                 /*  �����󥿺�Ȱ�                    */
  char              wk_jcl[BUFFSIZE];  /*  ��Ȱ�                            */

/* ************************************************************************* */
/*  ��ѥ����JCL��λ�ԥ����å�                                              */
/* ************************************************************************* */
/*  ���ԥ����ɤ�NULL���ꥢ  */
  strcpy( wk_jcl, jcl );
  memset( strchr(wk_jcl, '\n'), NULL, 1 );
/*  JCL �Ԥ����Х��Ȥξ��μ���  */
  if ( strlen( wk_jcl ) == 2 ) {
    if ( strcmp( wk_jcl, "//" ) == NULL )
      return( -1 );
  }
/*  JCL �Ԥ����Х��Ȥ�Ķ������μ���  */
  else if ( strlen( wk_jcl ) > 2 ) {
    if ( memcmp( wk_jcl, "//", 2 ) != NULL )
      return( 0 );
      /*  "//"�ʹߤ˶���ʳ���¸��̵ͭ����  */
    for ( i=2; i<strlen(wk_jcl); i++ ) {
      if ( memcmp( &wk_jcl[i], " ", 1 ) != NULL )
        return( 0 );
    }
    return( -1 );
  }
  return( 0 );
}
/* ************************************************************************* */
/*  ���ʥ⥸�塼��  Function list                                            */
/*                                                                           */
/*  used by mtlatr_, mtlalc_, mtlopn_, mtlred_, mtlwrt_,                     */
/*          mtlctl_, mtlcls_, mtlfre_, mtlend_                               */
/*                                                                           */
/*  - SND_data()                                                             */
/*    �ǡ�������                                                             */
/*  - RCV_rc()                                                               */
/*    ������̼���                                                           */
/*  - RCV_rcipf()                                                            */
/*    IPFCMD��̼���                                                         */
/*  - RCV_rcread()                                                           */
/*    READ�׵��̼�����READ������̤����READ-DATA��                        */
/*  - RCV_wait()                                                             */
/*    ���������Թ礻                                                         */
/*  - RCV_data()                                                             */
/*    �ǡ�������                                                             */
/*  - FLE_status()                                                           */
/*    �ե����륢������status����                                             */
/*                                                                           */
/* ************************************************************************* */
/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �ǡ�������                                              (C interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      int SND_data( int sok, char *data, int datalen )                     */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      sok         int         in           �����åȥǥ�������ץ�          */
/*      data        char *      in           �����ǡ�����Ǽ��                */
/*      datalen     int         in           �����ǡ���Ĺ                    */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int SND_data(sok, data, datalen)
  int sok;
  char *data;
  int datalen;
{
  int ret;

  /*  �����ѹ�����94.10.24    start                                          */
  /*  if ( send( sok, data, datalen ) < 0 )                                  */
  /* ----------------------------------------------------------------------- */
  if ( send( sok, data, datalen, 0 ) < 0 )
  /*  �����ѹ�����94.10.24    end                                            */
   return( ERTRNSSND );

  return( 0 );
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ������̼���                                            (C interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      int RCV_rc( int sok, int time )                                      */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      sok         int         in           �����åȥǥ�������ץ�          */
/*      time        int         in           ���������Թ礻���֡�ñ�̡��á�  */
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int RCV_rc(sok, time)
  int sok;
  int time;
{
  int ret, iwk;


  ret = RCV_wait( sok, time, '\n', 128, EXT_rvbf );
  if ( ret != 0 )
    return(ret);

  switch ( EXT_rvbf[0] ) {
    case 'A': return( NORMAL );
    case 'E':
      iwk = atoi( &EXT_rvbf[1] );
      return( iwk );
    default:
      return( ERTRNSPRO );
  }
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      IPFCMD��̼���                                          (C interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      int RCV_rcipf( int sok, int time, int *rc1, int *rc2 )               */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      sok         int         in           �����åȥǥ�������ץ�          */
/*      time        int         in           ���������Թ礻���֡�ñ�̡��á�  */
/*      rc1         int *       out          IPFCMD�����������ɣ�������������*/
/*      rc2         int *       out          IPFCMD�����������ɣ�������������*/
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int RCV_rcipf(sok, time, rc1, rc2)
  int sok;
  int time;
  int *rc1;
  int *rc2;
{
  int ret, iwk;

  *rc1 = 0;
  *rc2 = 0;

  ret = RCV_wait( sok, time, '\n', 128, EXT_rvbf );
  if ( ret != 0 )
    return( ret );

  switch ( EXT_rvbf[0] ) {
    case 'A': return( 0 );
    case 'E':
      iwk = atoi( &EXT_rvbf[1] );
      *rc1 = iwk;
      break;
    default:
      return( ERTRNSPRO );
  }

  ret = RCV_data( sok, '\n', 128, EXT_rvbf );
  if ( ret != 0 )
    return( ret );

  iwk = atoi( EXT_rvbf );
  *rc2 = iwk;

  return(ERIPFERR);
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      READ�׵��̼���(READ������̤����READ-DATA)           (C interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      int RCV_rcread( int sok, int time, char *readdata int *readlen )     */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      sok         int         in           �����åȥǥ�������ץ�          */
/*      time        int         in           ���������Թ礻���֡�ñ�̡��á�  */
/*      readdata    char *      out          �ɤ߹����ΰ�          ����������*/
/*      readlen     int  *      i/o          �ɤ߹����ΰ�Ĺ          ��������*/
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int RCV_rcread(sok, time, readdata, readlen)
  int sok;
  int time;
  char *readdata;
  int *readlen;
{
  int ret, iwk, reclen;

  ret = RCV_wait( sok, time, '\n', 128, EXT_rvbf );
  if ( ret != 0 )
    return( ret );

  switch ( EXT_rvbf[0] ) {
    case 'A':
      reclen = atoi( &EXT_rvbf[1] );
      if ( reclen == 0 )
        goto len_set;
      break;
    case 'E':
      iwk = atoi( &EXT_rvbf[1] );
      return( iwk );
    default:
      return( ERTRNSPRO );
  }

  if ( *readlen > reclen ) {
    ret = RCV_data( sok, NULL, reclen, readdata );
  } 
  else {
    ret = RCV_data( sok, NULL, *readlen, readdata );
  }
  if ( ret != 0 )
    return( ret );

len_set:;
  if ( reclen > *readlen ) {
    *readlen = reclen;
    return( ERBUFSIZE );
  }
  else {
    *readlen = reclen;
    return( NORMAL );
  }
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      ���������Թ礻                                          (C interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      int RCV_wait( int sok, int time, unsigned char lastchr,              */
/*                    int readlen, unsigned char readdata )                  */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      sok         int         in           �����åȥǥ�������ץ�          */
/*      time        int         in           ���������Թ礻���֡�ñ�̡��á�  */
/*      lastchr     u_char      in           ����ʸ��                ��������*/
/*      readlen     int  *      i/o          �ɤ߹����ΰ�Ĺ          ��������*/
/*      readdata    u_char     *out          �ɤ߹����ΰ�          ����������*/
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int RCV_wait(sok, time, lastchr, readlen, readdata)
  int sok;
  int time;
  unsigned char lastchr;
  int readlen;
  unsigned char *readdata;
{
  struct timeval timer;
  fd_set wk_mask;
  int    ret;

/*  �����Ԥ�����������  */
  timer.tv_sec=time;
  timer.tv_usec=0;
/*  �����Ԥ��ޥ�������  */
  FD_ZERO( &wk_mask );
  FD_SET( sok, &wk_mask );
/*  ��������ȯ���Թ礻  */
  ret = select( sok+1, &wk_mask, NULL, NULL, &timer );

  if ( ret == 0 ) {
    return( ERTIMEOUT );
  }
  else if ( ret < 0 ) {
    return( ERTRNSRCV );
  }
/*  �ǡ���������̤ˤ������  */
  return( RCV_data( sok, lastchr, readlen, readdata ) );
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �ǡ�������                                              (C interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      int RCV_data( int sok, unsigned char lastchr,                        */
/*                    int readlen, unsigned char readdata )                  */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      sok         int         in           �����åȥǥ�������ץ�          */
/*      lastchr     u_char      in           ����ʸ��                ��������*/
/*      readlen     int  *      i/o          �ɤ߹����ΰ�Ĺ          ��������*/
/*      readdata    u_char     *out          �ɤ߹����ΰ�          ����������*/
/*                                                                           */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int RCV_data(sok, lastchr, readlen, readdata)
  int sok;
  unsigned char lastchr;
  int readlen;
  unsigned char *readdata;
{
  unsigned char ch;
  int ret, readptr;

  readptr=0;
/*  ����ʸ����NULL�ξ��ν���(READ-DATA����)  */
  if ( lastchr == '\0' ) {
    for ( readptr = 0; readptr < readlen; readptr += ret ) {
      if ( readptr == readlen )
        return( NORMAL );
      ret = recv( sok, readdata + readptr, readlen - readptr, 0 );
      if ( ret <= 0 )
        return( ERTRNSRCV );
    }
#ifdef SNAP
  printf("RCV_data:%s#\n",readdata);
#endif
    return( NORMAL );
  }
/*  ����ʸ�������Ԥξ��ν���(res����)  */
  else {
    while ( (ret = recv( sok, &ch, 1, 0 )) > 0 ) {
      if ( ch == lastchr ) {
        readdata[readptr] = '\0';
#ifdef SNAP
  printf("RCV_data:%s#\n",readdata);
#endif
        return( NORMAL );
      }
      readdata[readptr++] = ch;
      if ( readptr == readlen )
        return( ERTRNSPRO );
    }
    if ( ret == 0 )
      return( ERTRNSCLS );
    else
      return( ERTRNSRCV );
  }
}

/* ************************************************************************* */
/*                                                                           */
/*  �ؿ�̾                                                                   */
/*      �ե����륢������status����                              (C interface)*/
/*                                                                           */
/*  ��  ��                                                                   */
/*      int FLE_status( int mode, int *unit, int action )                    */
/*                                                                           */
/*  �ѥ�᥿                                                                 */
/*     <̾��>       <��>       <I/O>        <����>                           */
/*      mode        int         in           �����⡼��                      */
/*                                           0:status�����å�                */
/*                                           1:status����                    */
/*      unit        int  *      in           ���ֻ����ֹ�            ��������*/
/*      action      int         in           �ե����륢�������ؼ���  ��������*/
/*                                           0:alloc 1:open 2:read 3:write   */
/*                                           4:rewind 5:backspace 6:endfile  */
/*                                           7:clode 8:free                  */
/*  �����                                                                   */
/*      0:���ｪλ                                                           */
/*    ��0:�۾ｪλ                                                           */
/*                                                                           */
/*  �ѹ�����                                                                 */
/*                                                                           */
/* ************************************************************************* */
int FLE_status( mode, unit, action )
 
  int   mode;
  int  *unit;
  int   action;

{
#ifdef SNAP
  int i;
  i = *unit;
  printf("FLE_status:EXT_status[%d]:%d#\n", 
          *unit, EXT_status[i] );
#endif

/*  status�����å�  */
  if ( mode == 0 ) {
    switch ( action ) {
      case( ALLOC ) : break;
      default     : {
        if ( EXT_status[*unit] == 0 )
          return( ERACTION );
        break;
      }
    }
  }
/*  status���󹹿�  */
  else {
    switch ( action ) {
      case( ALLOC ) : {
        EXT_status[*unit] = 1;
        break;
      }
      case( OPEN )  : {
        EXT_status[*unit] = 2;
        break;
      }
      case( CLOSE ) :
      case( FREE )  : {
        EXT_status[*unit] = 0;
        break;
      }
      default     : break;
    }
  }
    
#ifdef SNAP
  i = *unit;
  printf("FLE_status:EXT_status[%d]:%d#\n", 
          *unit, EXT_status[i] );
#endif

  return( NORMAL );
}
