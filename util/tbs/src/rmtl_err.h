/* ************************************************************************* */
/*                                                                           */
/*  Remote MTL access system : rmtl_err.h                                    */
/*                                                                           */
/*  ALLrights Reserved Copyright Fujitsu Limited 1994                        */
/*                                                                           */
/* ************************************************************************* */

/* ************************************************************************* */
/*   RMTL library error code list                                            */
/*                                                                           */
/*   CAUTION: cannot specify 20-29(FORTRAN IOS) and 70-79(GETMAIN for MSP)   */
/*                                                                           */
/* ************************************************************************* */
/*  #define  SNAP      1 */     /*  正常終了                                 */
#define  NORMAL      0          /*  正常終了                                 */
#define  ERTIMEOUT   4          /*  タイムアウト発生                         */
#define  ERSVNLNG   10          /*  サーバホスト(MSP) 名長さ不当             */
#define  ERSVNNOT   12          /*  サーバホスト(MSP) 名不当                 */
#define  ERSVNIP    13          /*  サーバホスト(MSP) 名形式不当             */
#define  ERTRANS    30          /*  転送処理中に異常発生                     */
#define  ERTRNSSOK  31          /*  転送用socket生成失敗                     */
#define  ERTRNSCON  32          /*  転送用connect失敗                        */
#define  ERTRNSSND  33          /*  転送（送信）失敗                         */
#define  ERTRNSRCV  34          /*  転送（受信）失敗                         */
#define  ERTRNSERR  35          /*  サーバホスト(MSP) 側で転送処理失敗(-rsp) */
#define  ERTRNSPRO  36          /*  プロトコルエラー                         */
#define  ERTRNSCLS  37          /*  close要求受付                            */
#define  ERFLNOT    40          /*  中継ジョブ定義ファイル無                 */
#define  ERFLACS    41          /*  中継ジョブ定義ファイルアクセス権不当     */
#define  ERFLLRL    42          /*  中継ジョブ定義ファイルレコード長不当     */
#define  ERFLUNEX   43          /*  中継ジョブ定義ファイルアクセス時エラー   */
#define  ERWSNGET   50          /*  自ホスト名取得失敗                       */
#define  ERWSIPGET  51          /*  自ホストIPアドレス取得失敗               */
#define  ERPORTSOK  52          /*  PORT取得用SOCKET生成失敗                 */
#define  ERPORTBIND 53          /*  PORT取得用BIND失敗                       */
#define  ERPORTGET  54          /*  POT番号取得失敗                          */
#define  ERJOBNGET  60          /*  ジョブ名取得失敗                         */
#define  ERJOBNLSN  61          /*  ジョブ名取得用listen失敗                 */
#define  ERJOBNACT  62          /*  ジョブ名取得用accept失敗                 */
#define  ERIPFERR   80          /*  IPFCMDのRC1,RC2にERROR CODEが設定された  */
#define  ERUNITNO   90          /*  装置参照番号不当                         */
#define  EROPERAND  91          /*  コマンドオペランド不当                   */
#define  ERMODE     92          /*  モード不当                               */
#define  ERLENGTH   93          /*  読込長、書込長に０が指定された           */
#define  ERBUFSIZE  94          /*  読込長とFORTRAN記録長が異なる            */
#define  ERACTION   95          /*  要求処理順序に誤りがある                 */

