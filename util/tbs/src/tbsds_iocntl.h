/*
** Definitions for tbsds C routines.
*/

struct tbsds_iocntl {
  int fd_receive;
  int fd_send;
  fd_set mask_rcv;
  fd_set mask_send;
};

static struct tbsds_iocntl tbsds_iocntl;

