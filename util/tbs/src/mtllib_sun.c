/* ************************************************************************* */
/*  Remote MTL access system : RMTL liblary                                  */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/* ************************************************************************* */
/*  Liblary  List                                                            */
/*                                                                           */
/*  - mtlint_()                                                               */
/*    中継ジョブ開始要求処理                                                 */
/*  - mtlatr_()                                                               */
/*    アトリビュート要求処理                                                 */
/*  - mtlalc_()                                                               */
/*    ＡＬＬＯＣ要求処理　　　　　                                           */
/*  - mtlopn_()                                                               */
/*    ＯＰＥＮ要求処理                                                       */
/*  - mtlred_()                                                               */
/*    ＲＥＡＤ要求処理　　　　　                                             */
/*  - mtlwrt_()                                                               */
/*    ＷＲＩＴＥ要求処理　　　                                               */
/*  - mtlctl_()                                                               */
/*    コントロール要求処理　　　                                             */
/*  - mtlcls_()                                                               */
/*    ＣＬＯＳＥ要求処理　　　                                               */
/*  - mtlfre_()                                                               */
/*    ＦＲＥＥ要求処理　　　　                                               */
/*  - mtlend_()                                                               */
/*    中継ジョブ終了要求処理　                                               */
/*                                                                           */
/* ************************************************************************* */
/* ***  仕様変更  94.05.24  start ****************************************** */
/*  1.  省略時追加ＤＤ文追加                                                 */
/*      //STDERR  DD  SYSOUT=*                                               */
/* ***  仕様変更  94.05.24  end   ****************************************** */

/* ***  仕様変更  94.06.18  start ****************************************** */
/*  1.  タイムアウト発生時のソケットクローズ処理追加                         */
/*  2.  ALLOC以外の待ち合わせ時間を延長　６０秒->１８０秒                    */
/* ***  仕様変更  94.06.18  end   ****************************************** */

/* ***  仕様変更  94.10.24  start ****************************************** */
/*  1.  関数SND_dataにおける、send関数修正　　　　　　　　　　　　　　　　　 */
/*      send( sok, data, datalen ) -> send( sok, data, datalen ,0 )          */
/* ***  仕様変更  94.10.24  end   ****************************************** */

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

/*  仕様変更  94.05.24  start  */
/*efine A_DDLINE           4           --  追加DD文行数                      */
/* ------------------------------------------------------------------------- */
#define A_DDLINE           5           /*  追加DD文行数                      */
/*  仕様変更  94.05.24  ended  */
#define STC_PORT (u_short)1994         /*  中継ジョブ起動処理ポート番号      */
#define PORT     (u_short)0            /*  空ポート番号取得時初期値          */
#define BUFFSIZE          82           /*  ファイルアクセス時バッファ長      */
#define SENDSIZE        1000           /*  送信バッファ長                    */
#define DATALEN          950           /*  JCL 送信時チェック長              */
#define ASYN               0           /*  非同期モード                      */
#define SYNC               1           /*  同期モード                        */
#define ACCEPT             0           /*  connect受信                       */
#define JCLcnf             1           /*  JCL 開始準備・終了結果受信        */
#define JOBind             2           /*  中継ジョブ名受信                  */
#define RCVLEN           512           /*  受信バッファ長                    */
/*  仕様変更  94.05.24  start  */
/*efine AD_DDN             3           --  省略時追加DD文数                  */
/* ------------------------------------------------------------------------- */
#define AD_DDN             4           /*  省略時追加DD文数                  */
/*  仕様変更  94.05.24  ended  */

#define OPRLEN        1024*2           /*  コマンドオペランドチェック長      */

#define ALCTIME          180           /*  ALLOC時待合せ時間（秒）           */
/*  仕様変更  94.06.18  start  */
/*efine TIME              60           --  ALLOC以外時待合せ時間             */
/* ------------------------------------------------------------------------- */
#define TIME             180           /*  ALLOC以外時待合せ時間             */
/*  仕様変更  94.06.18  ended  */

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
/*   外部変数域定義（全ライブラリ共通域）                                    */
/* ************************************************************************* */
static char     EXT_svip[16];               /*  サーバホスト(MSP)IPアドレス  */
static char     EXT_svnm[33];               /*  サーバホスト(MSP)名          */
static u_short  EXT_port    ;               /*  空ポート番号                 */
static int      EXT_sock    ;               /*  中継ジョブとのsocket         */
static char     EXT_jobn[9] ;               /*  中継ジョブ名                 */
static int      EXT_csok    ;               /*  中継ジョブとのconnect socket */
static fd_set   EXT_mask    ;               /*  非同期通信用マスク           */
static char     EXT_ownip[16];              /*  WSのIPアドレス               */

/* ************************************************************************* */
/*   共通域（MTLINITプロセスのみ)                                            */
/* ************************************************************************* */
static char         EXT_JCL[] = ".rmtljcl"; /*  中継ジョブ定義ファイル名     */
struct sockaddr_in  EXT_client;             /*  sockaddr_in構造体            */
struct def  {                               /*  省略時追加DD文構造体定義     */
       char         *srch_dd;               /*  1.検索用DD名                 */
       char         *add_ddn;               /*  2.追加DD文                   */
       int          flag;                   /*  3.中継JOB定義FILE内有無FLAG  */
};

                                           /*  省略時追加DD文構造体初期設定  */
static struct def   EXT_DDN[AD_DDN] = {
       "//FT06F001 ", "//FT06F001  DD  SYSOUT=*\n", 0,
       "//CMDIN "   , "//CMDIN     DD  DUMMY\n"   , 0,
       "//SYSPRINT ", "//SYSPRINT  DD  SYSOUT=*\n", 0,
/*  仕様変更  94.05.24  start  */
       "//STDERR "  , "//STDERR    DD  SYSOUT=*\n", 0
/*  仕様変更  94.05.24  ended  */
};

int      EXT_WAIT;                          /*  待ち時間                     */
char     EXT_RELAY[512];                    /*  採用中継ジョブ定義ファイル名 */

extern void mtl_sig();                      /*  シグナル受信時処理           */
/* ************************************************************************* */
/*   共通域（MTLINITプロセス以外)                                            */
/* ************************************************************************* */
static int      EXT_status[100];            /*  FILE-ACCESS制御TABLE         */
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
/*  関数名                                                                   */
/*      中継ジョブ開始要求処理                                               */
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlint_( char *hostname, int *time, char *jobname, int *rc )     */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      hostname    char *      in           ホスト名orIPアドレス            */
/*      time        int  *      in           タイマ値（秒単位）　　          */
/*      jobname     char *      out          中継ジョブ名                    */
/*      rc          int  *      out          復帰コード                      */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlint_( hostname, time, jobname, rc )

  char             *hostname;          /*  サーバホスト名                    */
  int              *time;              /*  WAITタイマ値                      */
  char             *jobname;           /*  中継ジョブ名                      */
  int              *rc;                /*  復帰コード                        */

{
  int               i;                 /*  作業域                            */
  char             *wp;                /*  作業域                            */
  int               len;

/* ************************************************************************* */
/*  外部変数域初期設定                                                       */
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
/*  主制御処理開始                                                           */
/* ************************************************************************* */
#ifdef  SNAP
  printf( "# mtlint_ start #\n" );
  printf( "mtlint_/hostname:%s#\n", hostname );
  printf( "mtlint_/time    :%d#\n", *time    );
#endif
/*  SIGPIPEシグナルを無視する。                                              */
/*  相手がsocketをCLOSEした後にsend, writeを実行すると、                     */
/*  プロセスが終了してしまう。以下に定義することにより回避可能               */
/*  send, write関数は-1を返す。                                              */
  signal( SIGPIPE, SIG_IGN );

/*  SIGINT(Cntl-D)及びSIGTERM(killコマンド)シグナル受付時処理設定            */
  signal( SIGINT, mtl_sig );
  signal( SIGTERM, mtl_sig );

/*  ホスト名長さチェック  */
  len = strlen( hostname );
  for ( i = len - 1; i >= 0; i-- ) {
    if ( hostname[i] != ' ' )  break;
  }
  if ( i == -1 || i > 32 ) {
    *rc = ERSVNLNG;
    return;
  }
/*  ホスト名を外部変数域へ設定  */
  memcpy( EXT_svnm, hostname, i + 1 );

#ifdef  SNAP
  printf( "mtlint_/EXT_svnm:%s#\n", EXT_svnm );
#endif
/*  タイマ値設定  */
  if ( *time < 10 )
    EXT_WAIT = 10;
  else
    EXT_WAIT = *time;

/*  中継ジョブJCL 定義ファイルチェック  */
  if ( (*rc = rmtljcl_chk( )) != NULL )
    return;
    
/*  中継ジョブJCL 転送  */
  if ( (*rc = JCL_trans( )) != NULL )
    return;
    
/*  中継ジョブJCL 転送処理が正常終了した場合、中継ジョブ名取得  */
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
/*  関数名                                                                   */
/*      アトリビュート要求処理                            (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlatr_( char *oper, int *rc, int *rc1, int *rc2 )               */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      oper        char *      in           ATTRIBコマンドオペランド        */
/*      rc          int  *      out          復帰コード          　          */
/*      rc1         int  *      out          IPFCMD実行時RC1                 */
/*      rc2         int  *      out          IPFCMD実行時RC2                 */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlatr_(oper, rc, rc1, rc2)
  char *oper;
  int *rc, *rc1, *rc2;
{
  int i;

/* コマンドオペランドチェック */

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
/*  ATTRIB指示子送信  */
  if ( (*rc=SND_data( EXT_sock, "T", 1 )) !=0 )  goto error;
/*  ATTRIBコマンドオペランド送信  */
  if ( (*rc=SND_data( EXT_sock, oper, i + 1 )) !=0 )  goto error;
/*  改行送信  */
  if ( (*rc=SND_data(EXT_sock, "\n", 1)) != 0 ) goto error;
/*  IPFCMD結果受信  */
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
/*  関数名                                                                   */
/*      アロケート要求処理                                (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlalc_( int *u, char *oper, int *rc, int *rc1, int *rc2 )       */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      u           int  *      in           装置参照番号        　          */
/*      oper        char *      in           ALLOCコマンドオペランド         */
/*      rc          int  *      out          復帰コード          　          */
/*      rc1         int  *      out          IPFCMD実行時RC1                 */
/*      rc2         int  *      out          IPFCMD実行時RC2                 */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
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

/* 装置参照番号範囲チェック */
  if ( *u<0 || *u==6 || *u>99 ) {
    *rc = ERUNITNO;
    return; 
  }

/* コマンドオペランドチェック  */
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
/*  ファイルアクセスstatusチェック  */
  if( (*rc=FLE_status( 0, u, ALLOC )) !=0 ) return;

/*  ALLOC指示子送信  */
  if ( (*rc=SND_data( EXT_sock, "A", 1 )) != 0 ) goto error;

/*  ALLOCコマンドオペランド送信  */
  if ( (*rc=SND_data( EXT_sock, oper, i + 1 )) !=0 ) goto error;

/*  装置参照番号より生成したDD名"F(FTxxF001)"送信            */
/*  ALLOCコマンドにおいて最終指定されたDD名が有効となるため  */
  sprintf( EXT_sdbf, " F(FT%.2dF001)\n", *u );
  if ( (*rc=SND_data( EXT_sock,
                      EXT_sdbf,
                      strlen(EXT_sdbf) )) !=0 ) goto error;
                      
/*  IPFCMD結果受信  */
  if ( (*rc=RCV_rcipf( EXT_sock, ALCTIME, rc1, rc2 )) !=0 ) {
    if ( *rc == ERIPFERR )  return;
    else goto error;
  }
  
/*  ファイルアクセスstatus更新  */
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
/*  関数名                                                                   */
/*      フリー要求処理                                    (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlfre_( int *u, char *oper, int *rc, int *rc1, int *rc2 )       */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      u           int  *      in           装置参照番号        　          */
/*      oper        char *      in           FREEコマンドオペランド          */
/*      rc          int  *      out          復帰コード          　          */
/*      rc1         int  *      out          IPFCMD実行時RC1                 */
/*      rc2         int  *      out          IPFCMD実行時RC2                 */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlfre_(u, oper, rc, rc1, rc2)
  int *u;
  char *oper;
  int *rc, *rc1, *rc2;
{
  int i, flg, cnt;
  char cnv_buff[3];

/*  装置参照番号範囲チェック  */
  if ( *u<-1 || *u==6 || *u>99 ) {
    *rc = ERUNITNO;
    return; 
  }

/*  コマンドオペランドチェック  */
  for ( i = strlen(oper) - 1; i >= 0; i-- ) {
    if ( oper[i] != ' ' )  break;
  }
  if ( i > OPRLEN - 13 ) {
    *rc = EROPERAND;
    return;
  }
/*  ファイルアクセスstatusチェック  */
  if ( *u != -1 ) {
    if( (*rc=FLE_status( 0, u, FREE )) !=0 )  return;
  }
/*  FREE指示子送信  */
  if( (*rc=SND_data( EXT_sock, "F", 1 )) !=0 )  goto error;
/*  FREEコマンドオペランド送信  */
  if ( i > -1 ) {
   if ( (*rc=SND_data( EXT_sock, oper, i + 1 )) !=0 )  goto error;
  }

/*  装置参照番号より生成したDD名"F(FTxxF001)"送信            */
/*  FREEコマンドにおいて最終指定されたDD名が有効となるため  */
  if ( *u != -1 )
    sprintf( EXT_sdbf, " F(FT%.2dF001)\n", *u );
  else
    sprintf( EXT_sdbf, "\n" );
  if ( (*rc=SND_data( EXT_sock, EXT_sdbf, strlen(EXT_sdbf) )) !=0 )
    goto error;
    
/*  IPFCMD結果受信  */
  if ( (*rc=RCV_rcipf( EXT_sock, TIME, rc1, rc2 )) !=0 ) {
    if ( *rc == ERIPFERR )  return;
    else goto error;
  }

/*  ファイルアクセスstatus更新  */
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
/*  関数名                                                                   */
/*      オープン要求処理                                  (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlopn_( int *u, char *opmode, int *rc )                         */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      u           int  *      in           装置参照番号        　          */
/*      opmode      char *      in           オープンモード                  */
/*                                           "READ" : 読み込み               */
/*                                           "WRITE": 書き込み               */
/*                                           "BOTH" : 読み書き               */
/*      rc          int  *      out          復帰コード          　          */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlopn_(u, opmode, rc)
  int *u;
  char *opmode;
  int *rc;
{
  int   mode;
  int   i;

/*  装置参照番号範囲チェック */
  if ( *u<0 || *u==6 || *u>99 ) {
    *rc = ERUNITNO;
    return; 
  }
 
#ifdef SNAP
  printf("opmode:%s#\n", opmode );
#endif

/*  オープンモードチェック  */
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
/*  ファイルアクセスstatusチェック  */
  if ( (*rc = FLE_status( 0, u, OPEN )) != 0 ) return;
/*  OPEN要求送信  */
  sprintf( EXT_sdbf, "O%d\n%d\n", *u, mode );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) !=0 ) goto error;
/*  OPEN処理結果受信  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) !=0 ) {
/*  仕様変更  94.06.18   start  */
/*  if ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) goto error;      */
/* --------------------------------------------------------------- */
    if ( ((*rc >= ERTRANS) && (*rc <= ERTRNSCLS)) || 
         ( *rc == ERTIMEOUT ) ) goto error;
/*  仕様変更  94.06.18   ended  */
    else return;
  }

/*  ファイルアクセスstatus更新  */
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
/*  関数名                                                                   */
/*      クローズ要求処理                                  (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlcls_( int *u, int *rc )                                       */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      u           int  *      in           装置参照番号        　          */
/*      rc          int  *      out          復帰コード          　          */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlcls_(u, rc)
  int *u;
  int *rc;
{
  int mode;

/*  装置参照番号範囲チェック */
  if ( *u < 0 || *u == 6 || *u > 99 ) {
    *rc = ERUNITNO;
    return; 
  }

/*  ファイルアクセスstatusチェック  */
  if ( (*rc = FLE_status( 0, u, CLOSE )) != 0 )
    return;
/*  CLOSE要求送信  */
  sprintf( EXT_sdbf, "C%d\n", *u );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto error;
/*  CLOSE処理結果受信  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) != 0 ) {
/*  仕様変更  94.06.18  start   */
/*  if ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) )              */
/*    goto error;                                              */
/* ----------------------------------------------------------- */
    if ( ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) ||
         ( *rc == ERTIMEOUT ) )
      goto error;
/*  仕様変更  94.06.18  ended   */
    return;
  }

/*  ファイルアクセスstatus更新  */
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
/*  関数名                                                                   */
/*      リード要求処理                                    (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlred_( int *u, char *buf, int *len, int *rc )                  */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      u           int  *      in           装置参照番号        　          */
/*      buf         char *      out          読み込み領域                    */
/*      len         int  *      i/o          読み込み領域長(in)              */
/*                                           1FORTRAN記録長(out)             */
/*      rc          int  *      out          復帰コード          　          */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlred_(u, buf, len, rc)
  int *u, *len, *rc;
  char *buf;
{
/*  装置参照番号範囲チェック */
  if ( *u < 0 || *u == 6 || *u > 99 ) {
    *rc = ERUNITNO;
    return; 
  }

/*  読み込み領域長チェック */
  if ( *len <= 0 ) {
    *rc = ERLENGTH;
    return;
  }

/* 読み込み領域の初期化 */
  memset(buf, '\0', *len);

/*  ファイルアクセスstatusチェック  */
  if ( (*rc = FLE_status( 0, u, READ )) != 0 )
    return;
/*  READ要求送信  */
  sprintf(EXT_sdbf, "R%d\n%d\n", *u, *len);
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto error;
/*  READ結果およびREAD-DATA受信  */
  if ( (*rc = RCV_rcread( EXT_sock, TIME, buf, len )) != 0 ) {
    if ( *rc == ERBUFSIZE )
      return;
    else {
/*  仕様変更  94.06.18   start   */
/*    if ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) )           */
/* ---------------------------------------------------------- */
      if ( ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) ||
           ( *rc == ERTIMEOUT ) )
/*  仕様変更  94.06.18   ended   */
        goto error;
      else
        return;
    }
  }

/*  ファイルアクセスstatus更新  */
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
/*  関数名                                                                   */
/*      ライト要求処理                                    (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlwrt_( int *u, char *buf, int *len, int *rc )                  */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      u           int  *      in           装置参照番号        　          */
/*      buf         char *      in           書き込みデータ格納領域          */
/*      len         int  *      in           書き込みデータ長                */
/*      rc          int  *      out          復帰コード          　          */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlwrt_(u, buf, len, rc)
  int *u, *len, *rc;
  char *buf;
{
/*  装置参照番号範囲チェック */
  if ( *u < 0 || *u == 6 || *u > 99 ) {
    *rc = ERUNITNO;
    return; 
  }

/*  書き込み領域長チェック */
  if ( *len <= 0 ) {
    *rc = ERLENGTH;
    return;
  }

/*  ファイルアクセスstatusチェック  */
  if ( (*rc = FLE_status( 0, u, WRITE )) != 0 )
    return;
/*  WRITE要求送信  */
  sprintf( EXT_sdbf, "W%d\n%d\n", *u, *len );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto error;
/*  WRITE準備結果受信  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) != 0 )
    goto error;
/*  書き込み領域(WRITE-DATA)送信  */
  if ( (*rc = SND_data( EXT_sock, buf, *len )) != 0 )
    goto error;
/*  WRITE結果受信  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) != 0 ) {
/*  仕様変更  94.06.18   start   */
/*  if ( (*rc>=ERTRANS) && (*rc<=ERTRNSCLS) )     */
/* ---------------------------------------------- */
    if ( ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) ||
         ( *rc == ERTIMEOUT ) )
/*  仕様変更  94.06.18   ended   */
      goto error;
    else
      return;
  }
  
/*  ファイルアクセスstatus更新  */
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
/*  関数名                                                                   */
/*      コントロール要求処理                              (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlctl_( int *u, char *action, int *rc )                         */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      u           int  *      in           装置参照番号        　          */
/*      action      char *      in           コントロール指示子              */
/*                                           "REWIND"    : リワインド        */
/*                                           "BACKSPACE" : バックスペース    */
/*                                           "ENDFILE"   : エンドファイル    */
/*      rc          int  *      out          復帰コード          　          */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlctl_(u, action, rc)
  int *u;
  char *action;
  int *rc;
{
  int mode;
  int i;

/*  装置参照番号範囲チェック */
  if ( *u < 0 || *u == 6 || *u > 99 ) {
    *rc = ERUNITNO;
    return; 
  }
/*  コントロール指示子チェック  */
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

/*  ファイルアクセスstatusチェック  */
  if ( (*rc = FLE_status( 0, u, mode+4 )) != 0 )
    return;
/*  コントロール要求送信  */
  sprintf( EXT_sdbf, "I%d\n%d\n", *u, mode );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto error;
/*  コントロール結果受信  */
  if ( (*rc = RCV_rc( EXT_sock, TIME )) != 0 ) {
/*  仕様変更  94.06.18   start   */
/*  if ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) )       */
/* ---------------------------------------------------- */
    if ( ( (*rc >= ERTRANS) && (*rc <= ERTRNSCLS) ) ||
         ( *rc == ERTIMEOUT ) )
/*  仕様変更  94.06.18   ended   */
      goto error;
    else
      return;
  }

/*  ファイルアクセスstatus更新  */
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
/*  関数名                                                                   */
/*      中継ジョブ終了要求処理                            (FORTRAN interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      void mtlend_( int *rc )                                               */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      rc          int  *      out          復帰コード          　          */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void mtlend_(rc)
  int *rc;
{
/*  中継ジョブ終了要求送信  */
  sprintf( EXT_sdbf, "E\n" );
  if ( (*rc = SND_data( EXT_sock,
                        EXT_sdbf,
                        strlen( EXT_sdbf ) )) != 0 )
    goto ende;
/*  サーバからのsocketクローズ待合せ  */
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
/*  関数名                                                                   */
/*      シグナル受信時処理                                                   */
/*                                                                           */
/*  書  式                                                                   */
/*      void  mtl_sig( )                                                     */
/*                                                                           */
/*  パラメタ                                                                 */
/*      なし                                                                 */
/*                                                                           */
/*  戻り値                                                                   */
/*      なし                                                                 */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
void  mtl_sig()
{
/* ************************************************************************* */
/*   Cntl-C  及び  killコマンド受信時処理                                    */
/* ************************************************************************* */
  exit( 0 );
}

/* ************************************************************************* */
/*  部品モジュール  Function list                                            */
/*                                                                           */
/*  used by mtlint_                                                           */
/*                                                                           */
/*  - JCL_trans()                                                            */
/*    中継ジョブ転送処理                                                     */
/*  - rmtljcl_chk()                                                          */
/*    中継ジョブ定義ファイルチェック                                         */
/*  - ip_port_get()                                                          */
/*    自IPアドレスw繼sポート番号取得                                          */
/*  - JCL_send()                                                             */
/*    中継ジョブJCL転送                                                      */
/*  - JCL_data()                                                             */
/*    中継ジョブ定義ファイル内JCLおよび追加DD文転送                          */
/*  - jobname_recv()                                                         */
/*    中継ジョブ名をサーバホスト(MSP)より受信                                */
/*  - data_rcv_anl()                                                         */
/*    サーバホスト(MSP)からの送信データ受信w繪��                            ��
��  裔赧竏繝襾                                                           ��
��    中継ジョブJCL終了行チェック                                            */
/*                                                                           */
/* ************************************************************************* */
/* ************************************************************************* */
/*                                                                           */
/*  関数名                                                                   */
/*      中継ジョブ定義ファイルチェック                                       */
/*                                                                           */
/*  書  式                                                                   */
/*      int rmtljcl_chk( )                                                   */
/*                                                                           */
/*  パラメタ                                                                 */
/*      なし                                                                 */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  rmtljcl_chk( )

{
  FILE             *JCLFILE;           /*  中継ジョブ定義ファイルポインタ    */
  struct stat       filemode;          /*  ファイルモード構造体              */
  u_short           mode;              /*  ファイルモード                    */
  char              buff[BUFFSIZE];    /*  ファイルREADバッファ              */
  char             *file;              /*  中継ジョブ定義ファイル名ポインタ  */

/* ************************************************************************* */
/*  中継ジョブ定義ファイルチェック                                           */
/* ************************************************************************* */
#ifdef  SNAP
  printf( "# rmtljcl_chk start #\n" );
#endif
/*  環境変数に定義されている場合、定義ファイル名を採用  */
/*  定義されていない場合、カレントディレクトリ配下の".rmtljcl"採用 */
  if ( (file=getenv( "RMTLJCL" )) != NULL )
    strcpy( EXT_RELAY, file );
  else
    strcpy( EXT_RELAY, EXT_JCL );

/*  環境変数にnull定義されている場合、
　　カレントディレクトリ配下の".rmtljcl"採用  */
  if ( strlen( EXT_RELAY ) == 0 )
    strcpy( EXT_RELAY, EXT_JCL );

#ifdef  SNAP
  printf( "rmtljcl_chk/EXT_RELAY:%s#\n", EXT_RELAY );
#endif

/*  定義ファイル有無チェック  */
  if ( access( EXT_RELAY, 0 ) != NULL )
    return( ERFLNOT );

/*  ファイルモード取得  */
  if ( stat( EXT_RELAY, &filemode ) != NULL )
    return( ERFLACS );

/*  ファイルアクセス権チェック（他者からのアクセス権がないか）  */
  mode = filemode.st_mode;
  mode = mode & S_IRWXO;
  if ( mode != NULL )
    return( ERFLACS );

  mode = filemode.st_mode;
  mode = mode & S_IRWXG;
  if ( mode != NULL )
    return( ERFLACS );

/*  ファイルレコード長チェック  */
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
/*  関数名                                                                   */
/*      中継ジョブJCL 転送                                                   */
/*                                                                           */
/*  書  式                                                                   */
/*      int JCL_trans( )                                                     */
/*                                                                           */
/*  パラメタ                                                                 */
/*      なし                                                                 */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  JCL_trans( )

{
  struct hostent     *MSP;             /*  hostent構造体                     */
  struct sockaddr_in  server;          /*  sockaddr_in構造体                 */
  int                 i, j;            /*  作業域                            */
  char                wk_host[40];     /*  サーバホスト名作業域              */
  char               *wp, *fp;         /*  IPアドレスチェック時作業ポインタ  */
  unsigned int        u_add;
  unsigned char       uchr[4];
  int                 s_sok;           /*  サーバホスト(MSP)とのsocket       */
 
/* ************************************************************************* */
/*  中継ジョブJCL 転送                                                       */
/* ************************************************************************* */
#ifdef  SNAP
  printf( "# JCL_trans start #\n" );
  printf( "JCL_trans/EXT_svnm:%s#\n", EXT_svnm );
#endif
/*  ホスト名かIPアドレス指定か識別  */
  wp = EXT_svnm;
  for ( i=0; i<3; i++,wp++ ) {
    if ( (wp=strchr( wp, '.' )) == NULL )
      break;
  }
/*  IPアドレスとして"."の数が正しい  */
  if ( i == 3 ) {
/*  IPアドレス形式チェック  */
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
/*  ホスト名指定時処理  */
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

/*  サーバホスト(MSP)とのconnect確立  */
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

/*  自IPアドレスw繼sポート番号取得  */
  if ( (i=ip_port_get( s_sok, server )) != NULL ) {
    close( s_sok );
    return( i );
  }

  FD_ZERO( &EXT_mask );
  FD_SET( s_sok, &EXT_mask );

/*  中継ジョブJCL 転送  */
  i=JCL_send( s_sok );
  close( s_sok );

#ifdef  SNAP
  printf( "# JCL_trans ended #\n" );
#endif

  return( i );
}

/* ************************************************************************* */
/*                                                                           */
/*  関数名                                                                   */
/*      自IPアドレスw繼sポート番号取得                                        */
/*                                                                           */
/*  書  式                                                                   */
/*      int ip_port_get( s_sok )                                             */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      s_sok       int         in           中継ジョブJCL 転送用socket      */
/*      server      struct      in           中継ジョブJCL 転送用sockaddr_in */
/*                  sockaddr_in                                              */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  ip_port_get( s_sok, server )

  int                 s_sok;
  struct sockaddr_in  server;

{
  int                 len;             /*  sockaddr_in構造体長               */

/* ************************************************************************* */
/*  自IPアドレスw繼sポート番号取得                                            */
/* ************************************************************************* */
#ifdef  SNAP
  printf( "# ip_port_get start #\n" );
#endif

/*  自IPアドレス取得  */
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

/*  空ポート番号および中継ジョブ名とのconnectディスクリプタ取得  */
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
/*  関数名                                                                   */
/*      JCL 転送処理                                                         */
/*                                                                           */
/*  書  式                                                                   */
/*      int JCL_send( s_sok )                                                */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      s_sok       int         in           中継ジョブJCL 転送用socket      */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  JCL_send( s_sok )

  int   s_sok;

{
  FILE             *JCLFILE;           /*  中継ジョブ定義ファイルポインタ    */
  struct passwd    *uid;               /*  ユーザID構造体                    */
  char              buff[BUFFSIZE];    /*  ファイルREADバッファ              */
  char              sendbuf[SENDSIZE]; /*  送信バッファ                      */
  char              readybf[SENDSIZE]; /*  送信用準備バッファ                */
  int               jcl_line;          /*  中継ジョブJCL 行数                */
  int               add_line;          /*  省略時追加DD文行数                */
  char              w_char[80];        /*  作業域文字列域                    */
  int               len;               /*  作業域長さ                        */
  int               rtcode;            /*  復帰コード                        */
  int               prim;              /*  受信データ種別                    */
  char             *dd_area;           /*  追加DD文作成域ポインタ            */
  int               i;                 /*  カウンタ作業域                    */

/* ************************************************************************* */
/*  JCL 転送処理                                                             */
/* ************************************************************************* */
#ifdef  SNAP
  printf( " # JCL_send start/s_sok:%d#\n", s_sok );
#endif

/*  WSのユーザID取得  */
  if ( (uid=getpwuid( getuid() )) == NULL )
    return( ERTRANS );

#ifdef  SNAP
  printf( "JCL_send/own userid:%s#\n", uid->pw_name );
#endif

/*  中継ジョブ定義ファイル行数カウント  */
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
    /*  省略時追加DD文有無検索  */
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

/*  追加DD文(DD名:MTLIOINF及び省略時追加DD文)作成  */
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

/*  JCL 開始要求送信  */
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

/*  JCL 開始応答受信  */
  rtcode = data_rcv_anl( s_sok, SYNC, &prim );

#ifdef  SNAP
  printf( "JCL_send/JCLcnf recv rtcode:%d\n", rtcode );
  printf( "JCL_send/JCLcnf recv prim  :%d\n", prim   );
#endif

  switch ( rtcode ) {
    case( 0 ):                              /*  正常終了       */
      {
         if ( prim != JCLcnf )
           return( ERTRANS );
         break;
      }
    case( 1 ):                              /*  -cnf 受信       */
        return( ERTRNSERR );
    case( 2 ):                              /*  プロトコルエラー*/
        return( ERTRNSPRO );
    case( -1 ):                             /*  異常終了         */
        return( ERTRNSRCV );
    case( -2 ):                             /*  タイムアウト発生  */
        return( ERTIMEOUT );
  }

/*  JCL データ転送  */
  if ( (rtcode=JCL_data( s_sok, dd_area )) != NULL ) {
    free( dd_area );
    return( rtcode );
  }
  free( dd_area );

/*  JCL 終了要求送信  */
  strcpy( sendbuf, "J2\n0\n" );

#ifdef  SNAP
  printf( "JCL_send/JCL-ENDreq:%s", sendbuf );
#endif

  if ( send( s_sok, sendbuf, strlen(sendbuf),0 ) < 0 )
    return( ERTRNSSND );

/*  JCL 終了応答受信  */
  rtcode = data_rcv_anl( s_sok, SYNC, &prim );

#ifdef  SNAP
  printf( "JCL_send/JCL-ENDcnf recv rtcode:%d\n", rtcode );
  printf( "JCL_send/JCL-ENDcnf recv prim  :%d\n", prim   );
#endif

  switch ( rtcode ) {
    case( 0 ):                              /*  正常終了       */
      {
         if ( prim != JCLcnf )
           return( ERTRANS );
         break;
      }
    case( 1 ):                              /*  -cnf 受信       */
        return( ERTRNSERR );
    case( 2 ):                              /*  プロトコルエラー*/
        return( ERTRNSPRO );
    case( -1 ):                             /*  異常終了         */
        return( ERTRNSRCV );
    case( -2 ):                             /*  タイムアウト発生  */
        return( ERTIMEOUT );
  }

#ifdef  SNAP
  printf( "# JCL_send ended #\n" );
#endif

/*  正常終了  */
  return( NORMAL );
}

/* ************************************************************************* */
/*                                                                           */
/*  関数名                                                                   */
/*      JCL データ転送                                                       */
/*                                                                           */
/*  書  式                                                                   */
/*      int JCL_data( s_sok, dd_area )                                       */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      s_sok       int         in           中継ジョブJCL 転送用socket      */
/*      dd_area     char *      in           追加DD文格納域                  */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*     -1:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  JCL_data( s_sok, dd_area )

  int    s_sok;
  char  *dd_area;

{
                                       /*  JCL データHEADER雛形              */
  static char      header[]="J1\n1000\n";
  FILE             *JCLFILE;           /*  中継ジョブ定義ファイルポインタ    */
  char              buff[BUFFSIZE];    /*  ファイルREADバッファ              */
  char              sendbuf[SENDSIZE]; /*  送信バッファ                      */
  char              readybf[SENDSIZE]; /*  送信用準備バッファ                */
  int               len;               /*  長さ用作業域                      */
  int               send_flag;         /*  送信フラグ 0:未送信 1:送信済      */
  int               rtcode;            /*  復帰コード                        */

/* ************************************************************************* */
/*  JCL データ転送                                                           */
/* ************************************************************************* */

#ifdef  SNAP
  printf( "# JCL_data start #\n" );
#endif

/*  中継ジョブ定義ファイルOPEN  */
  if ( (JCLFILE=fopen( EXT_RELAY, "r" )) == NULL )
    return( ERTRANS );
  
/*  中継ジョブJCL 転送  */
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

/*  追加DD文転送  */
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

/*  正常終了  */
  return( NORMAL );
}

/* ************************************************************************* */
/*                                                                           */
/*  関数名                                                                   */
/*      中継ジョブ名受信                                                     */
/*                                                                           */
/*  書  式                                                                   */
/*      int jobname_recv( )                                                  */
/*                                                                           */
/*  パラメタ                                                                 */
/*      なし                                                                 */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*      4:タイムアウト                                                       */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  jobname_recv( )

{
  int               rtcode;            /*  復帰コード                        */
  int               size;              /*  作業域                            */
  int               prim;              /*                                    */
 
/* ************************************************************************* */
/*  中継ジョブ名受信                                                         */
/* ************************************************************************* */

#ifdef  SNAP
  printf( "# jobname_recv start #\n" );
#endif

/*  中継ジョブからのconnect待ちキュー設定  */
  if ( listen( EXT_csok, 1 ) < 0 ) {
    close( EXT_csok );
    return( ERJOBNLSN );
  }
/*  中継ジョブからのconnect待合わせ  */
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

/*  中継ジョブから中継ジョブ名受信  */
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
/*  関数名                                                                   */
/*      データ受信解析                                                       */
/*                                                                           */
/*  書  式                                                                   */
/*      int data_rcv_anl( w_sok, mode, prim )                                */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      w_sok       int         in           ソケットディスクリプタ          */
/*      mode        int         in           受信モード                      */
/*      prim        int  *      out          受信種別                        */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*      1:-cnf受信                                                           */
/*      2:プロトコルエラー                                                   */
/*     -1:異常終了                                                           */
/*     -2:タイムアウト                                                       */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  data_rcv_anl( w_sok, mode, prim )

  int               w_sok;             /*  ソケットディスクリプタ            */
  int               mode;              /*  受信モード                        */
  int              *prim;              /*  受信種別                          */

{
  fd_set            wk_mask;           /*  select用作業マスク域              */
  struct timeval    timer;             /*  selectタイマ用構造体              */
  char              rcvbuf[RCVLEN];    /*  受信データ格納域                  */
  int               rcvlen;            /*  受信データ長                      */
  int               srch_sd;           /*  検索ディスクリプタw繝rット         */
  int               rtcode;            /*  復帰コード                        */
  char             *wp;                /*                                    */
  int               len;               /*                                    */

/* ************************************************************************* */
/*  データ受信・解析　                                                       */
/* ************************************************************************* */
/*  selectマスクcopy  */
  wk_mask = EXT_mask;
/*  検索ディスクリプタ・ビット設定  */
  srch_sd = w_sok + 1;
/*  非同期モード処理  -  指定時間事象待合せ  */
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
/*  同期モード処理  - 事象発生まで待合せ  */
    if ( select( srch_sd, &wk_mask, NULL, NULL, NULL ) < 0 )
      return( -1 );
  }

/*  受信データ検索  */
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

/*  受信データ解析  */
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

/*  正常終了  */
  return( 0 );
}
/* ************************************************************************* */
/*                                                                           */
/*  関数名                                                                   */
/*      中継ジョブJCL終了行チェック                                          */
/*                                                                           */
/*  書  式                                                                   */
/*      int jcl_check( jcl )                                                 */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      jcl         char *      in           JCL-READ行                      */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:終了行ではない                                                     */
/*     -1:終了行である                                                       */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int  jcl_check( jcl )

  char             *jcl;               /*  JCL-READ行                        */

{
  int               i;                 /*  カウンタ作業域                    */
  char              wk_jcl[BUFFSIZE];  /*  作業域                            */

/* ************************************************************************* */
/*  中継ジョブJCL終了行チェック                                              */
/* ************************************************************************* */
/*  改行コードをNULLクリア  */
  strcpy( wk_jcl, jcl );
  memset( strchr(wk_jcl, '\n'), NULL, 1 );
/*  JCL 行が２バイトの場合の識別  */
  if ( strlen( wk_jcl ) == 2 ) {
    if ( strcmp( wk_jcl, "//" ) == NULL )
      return( -1 );
  }
/*  JCL 行が２バイトを超える場合の識別  */
  else if ( strlen( wk_jcl ) > 2 ) {
    if ( memcmp( wk_jcl, "//", 2 ) != NULL )
      return( 0 );
      /*  "//"以降に空白以外の存在有無識別  */
    for ( i=2; i<strlen(wk_jcl); i++ ) {
      if ( memcmp( &wk_jcl[i], " ", 1 ) != NULL )
        return( 0 );
    }
    return( -1 );
  }
  return( 0 );
}
/* ************************************************************************* */
/*  部品モジュール  Function list                                            */
/*                                                                           */
/*  used by mtlatr_, mtlalc_, mtlopn_, mtlred_, mtlwrt_,                     */
/*          mtlctl_, mtlcls_, mtlfre_, mtlend_                               */
/*                                                                           */
/*  - SND_data()                                                             */
/*    データ送信                                                             */
/*  - RCV_rc()                                                               */
/*    処理結果受信                                                           */
/*  - RCV_rcipf()                                                            */
/*    IPFCMD結果受信                                                         */
/*  - RCV_rcread()                                                           */
/*    READ要求結果受信（READ処理結果およびREAD-DATA）                        */
/*  - RCV_wait()                                                             */
/*    受信事象待合せ                                                         */
/*  - RCV_data()                                                             */
/*    データ受信                                                             */
/*  - FLE_status()                                                           */
/*    ファイルアクセスstatus制御                                             */
/*                                                                           */
/* ************************************************************************* */
/* ************************************************************************* */
/*                                                                           */
/*  関数名                                                                   */
/*      データ送信                                              (C interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      int SND_data( int sok, char *data, int datalen )                     */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      sok         int         in           ソケットディスクリプタ          */
/*      data        char *      in           送信データ格納域                */
/*      datalen     int         in           送信データ長                    */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
/*                                                                           */
/* ************************************************************************* */
int SND_data(sok, data, datalen)
  int sok;
  char *data;
  int datalen;
{
  int ret;

  /*  仕様変更　　94.10.24    start                                          */
  /*  if ( send( sok, data, datalen ) < 0 )                                  */
  /* ----------------------------------------------------------------------- */
  if ( send( sok, data, datalen, 0 ) < 0 )
  /*  仕様変更　　94.10.24    end                                            */
   return( ERTRNSSND );

  return( 0 );
}

/* ************************************************************************* */
/*                                                                           */
/*  関数名                                                                   */
/*      処理結果受信                                            (C interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      int RCV_rc( int sok, int time )                                      */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      sok         int         in           ソケットディスクリプタ          */
/*      time        int         in           受信事象待合せ時間（単位：秒）  */
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
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
/*  関数名                                                                   */
/*      IPFCMD結果受信                                          (C interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      int RCV_rcipf( int sok, int time, int *rc1, int *rc2 )               */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      sok         int         in           ソケットディスクリプタ          */
/*      time        int         in           受信事象待合せ時間（単位：秒）  */
/*      rc1         int *       out          IPFCMDの復帰コード１　　　　　　*/
/*      rc2         int *       out          IPFCMDの復帰コード２　　　　　　*/
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
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
/*  関数名                                                                   */
/*      READ要求結果受信(READ処理結果およびREAD-DATA)           (C interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      int RCV_rcread( int sok, int time, char *readdata int *readlen )     */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      sok         int         in           ソケットディスクリプタ          */
/*      time        int         in           受信事象待合せ時間（単位：秒）  */
/*      readdata    char *      out          読み込み領域          　　　　　*/
/*      readlen     int  *      i/o          読み込み領域長          　　　　*/
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
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
/*  関数名                                                                   */
/*      受信事象待合せ                                          (C interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      int RCV_wait( int sok, int time, unsigned char lastchr,              */
/*                    int readlen, unsigned char readdata )                  */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      sok         int         in           ソケットディスクリプタ          */
/*      time        int         in           受信事象待合せ時間（単位：秒）  */
/*      lastchr     u_char      in           検索文字                　　　　*/
/*      readlen     int  *      i/o          読み込み領域長          　　　　*/
/*      readdata    u_char     *out          読み込み領域          　　　　　*/
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
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

/*  事象待ちタイマ設定  */
  timer.tv_sec=time;
  timer.tv_usec=0;
/*  事象待ちマスク設定  */
  FD_ZERO( &wk_mask );
  FD_SET( sok, &wk_mask );
/*  受信事象発生待合せ  */
  ret = select( sok+1, &wk_mask, NULL, NULL, &timer );

  if ( ret == 0 ) {
    return( ERTIMEOUT );
  }
  else if ( ret < 0 ) {
    return( ERTRNSRCV );
  }
/*  データ受信結果により復帰  */
  return( RCV_data( sok, lastchr, readlen, readdata ) );
}

/* ************************************************************************* */
/*                                                                           */
/*  関数名                                                                   */
/*      データ受信                                              (C interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      int RCV_data( int sok, unsigned char lastchr,                        */
/*                    int readlen, unsigned char readdata )                  */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      sok         int         in           ソケットディスクリプタ          */
/*      lastchr     u_char      in           検索文字                　　　　*/
/*      readlen     int  *      i/o          読み込み領域長          　　　　*/
/*      readdata    u_char     *out          読み込み領域          　　　　　*/
/*                                                                           */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
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
/*  検索文字がNULLの場合の処理(READ-DATA受信)  */
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
/*  検索文字が改行の場合の処理(res受信)  */
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
/*  関数名                                                                   */
/*      ファイルアクセスstatus制御                              (C interface)*/
/*                                                                           */
/*  書  式                                                                   */
/*      int FLE_status( int mode, int *unit, int action )                    */
/*                                                                           */
/*  パラメタ                                                                 */
/*     <名称>       <型>       <I/O>        <用途>                           */
/*      mode        int         in           処理モード                      */
/*                                           0:statusチェック                */
/*                                           1:status更新                    */
/*      unit        int  *      in           装置参照番号            　　　　*/
/*      action      int         in           ファイルアクセス指示子  　　　　*/
/*                                           0:alloc 1:open 2:read 3:write   */
/*                                           4:rewind 5:backspace 6:endfile  */
/*                                           7:clode 8:free                  */
/*  戻り値                                                                   */
/*      0:正常終了                                                           */
/*    ≠0:異常終了                                                           */
/*                                                                           */
/*  変更履歴                                                                 */
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

/*  statusチェック  */
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
/*  status情報更新  */
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
