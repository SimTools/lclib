/*****************************************************
**  To read VBS data created on FACOM MSP
**
**  Coded by A.Miyamoto  15-Feb-1994
**
******************************************************/

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include <fcntl.h>

struct comtype {
  FILE *instream[2];
  int fd[2];
  long doread;
};
struct comtype vread_cntl_;

/*************************************************
**  Function prototypes 
*************************************************/
void tbvopn_(); /*   Open data file.  */
/***************************************
**
**  Open file to read data
**
***************************************/

void tbvopn_(filein, lenf)
char *filein;
int lenf;
{
  char  filename[200];

  strncpy(&filename[0],filein,lenf);
  strncpy(&filename[lenf],"\0 ",2); 

/*  printf("Input file name is %s \n",filename); */

  if ( strcmp(filename,"PIPE") != 0 ){
    if( (vread_cntl_.instream[1]=fopen(filename,"rb")) == NULL ){
/*    if( (vread_cntl_.fd[1]=open(filename,O_RDONLY)) == -1 ){*/
      printf(" in TBVOPN...Error to open file \n");  
      exit(1);
    }  /* Open new file, binary mode */
  
  }
  else{
    if( (pipe(vread_cntl_.fd)) != 0 ){
      printf(" in VOPEN... can not open input pipe. \n");
      exit(1);
    }
    else{
      printf("Pipe is opened sucessfuly \n");
    }
  }

/*  printf(" File opened successfull \n"); */
  vread_cntl_.doread = 0;

}
      
