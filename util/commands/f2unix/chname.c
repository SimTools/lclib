/*****************************************************************************
**  chname.c
** ==< Function >===
**   Change FACOM standard naming convension to those suitable
**   for unix.
**
** ==< Command format >==
**   chname 
**   
**   if command is executed without any option, input is obtained from stdin.
**   unix file name is written to stdout
**
** ==< Conversion rule >==
**   (1) A character $ are converted to x.
**   (2) @XXXXX -> XXXXX.inc
**   (3) If not @XXXXX, #XXXXX, change to XXXXX.f
**
** ==< Author >==
**   A. Miyamoto  19-Feb-1994  Original version.
**
****************************************************************************/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void  Chname();

main(argc, argv)
int argc;
char *argv[];
{
  char *instr[163];
  char *outstr[163];

  if(argc==2){
      Chname(argv[2],outstr);
      puts(outstr);
    }
  else if(argc==1){
    while( gets(instr) != NULL ){
      Chname(instr, outstr);
      puts(outstr);
      
    }}
  else {
    system("more /net/topsun1/home/topsun1/miyamoto/TOPAZLIB/f2unix/chname.doc");
  }

 exit;
}

    

/*
** (2.2) change $ to x
*/
/*    printf("argv=%s\n",argv[1]); */
void Chname(instr,outstr)
char *instr;
char *outstr;
{
   int lstr,i;

/*   printf("instr=%s ",instr);*/

   lstr=strlen(instr);
     for(i=0;i<lstr;i++){
       if(strncmp(instr+i,"$",1)==0){ 
	 strncpy(instr+i,"x",1);
       }
     }
   
    if(strncmp(instr,"@",1)==0){
      if( strncmp(instr,"@#",2)==0){
	strcpy(outstr,"#");
        strncpy(outstr,instr+1,lstr-1);
        strncpy(outstr+lstr-1,"\0",1);
        strcat(outstr,".lst\0");
      }
      else{
        strcpy(outstr," ");
        strncpy(outstr,instr+1,lstr-1);
        strncpy(outstr+lstr-1,"\0",1);
        strcat(outstr,".inc\0");
      }
    }
    else{
      strcpy(outstr,instr);
      if(strncmp(instr,"#",1)!=0){
	strcat(outstr,".f");}
    }

  return;
}
