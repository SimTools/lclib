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
/*  #define  SNAP      1 */     /*  ���ｪλ                                 */
#define  NORMAL      0          /*  ���ｪλ                                 */
#define  ERTIMEOUT   4          /*  �����ॢ����ȯ��                         */
#define  ERSVNLNG   10          /*  �����Хۥ���(MSP) ̾Ĺ������             */
#define  ERSVNNOT   12          /*  �����Хۥ���(MSP) ̾����                 */
#define  ERSVNIP    13          /*  �����Хۥ���(MSP) ̾��������             */
#define  ERTRANS    30          /*  ž��������˰۾�ȯ��                     */
#define  ERTRNSSOK  31          /*  ž����socket��������                     */
#define  ERTRNSCON  32          /*  ž����connect����                        */
#define  ERTRNSSND  33          /*  ž���������˼���                         */
#define  ERTRNSRCV  34          /*  ž���ʼ����˼���                         */
#define  ERTRNSERR  35          /*  �����Хۥ���(MSP) ¦��ž����������(-rsp) */
#define  ERTRNSPRO  36          /*  �ץ�ȥ��륨�顼                         */
#define  ERTRNSCLS  37          /*  close�׵����                            */
#define  ERFLNOT    40          /*  ��ѥ��������ե�����̵                 */
#define  ERFLACS    41          /*  ��ѥ��������ե����륢������������     */
#define  ERFLLRL    42          /*  ��ѥ��������ե�����쥳����Ĺ����     */
#define  ERFLUNEX   43          /*  ��ѥ��������ե����륢�����������顼   */
#define  ERWSNGET   50          /*  ���ۥ���̾��������                       */
#define  ERWSIPGET  51          /*  ���ۥ���IP���ɥ쥹��������               */
#define  ERPORTSOK  52          /*  PORT������SOCKET��������                 */
#define  ERPORTBIND 53          /*  PORT������BIND����                       */
#define  ERPORTGET  54          /*  POT�ֹ��������                          */
#define  ERJOBNGET  60          /*  �����̾��������                         */
#define  ERJOBNLSN  61          /*  �����̾������listen����                 */
#define  ERJOBNACT  62          /*  �����̾������accept����                 */
#define  ERIPFERR   80          /*  IPFCMD��RC1,RC2��ERROR CODE�����ꤵ�줿  */
#define  ERUNITNO   90          /*  ���ֻ����ֹ�����                         */
#define  EROPERAND  91          /*  ���ޥ�ɥ��ڥ�������                   */
#define  ERMODE     92          /*  �⡼������                               */
#define  ERLENGTH   93          /*  �ɹ�Ĺ�����Ĺ�ˣ������ꤵ�줿           */
#define  ERBUFSIZE  94          /*  �ɹ�Ĺ��FORTRAN��ϿĹ���ۤʤ�            */
#define  ERACTION   95          /*  �׵��������˸�꤬����                 */

