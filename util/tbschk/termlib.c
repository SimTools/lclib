/*
#include <curses.h>
*/
#ifdef __DARWIN__
#include "ncurses.h"
#else
#include "curses.h"
#endif
#define CTRL(c) ('c'&037)

void tinit_()
{
  initscr();
  cbreak();
  nonl();
  idlok(stdscr, TRUE);
  keypad(stdscr, TRUE);
  move(0,0);
  noecho();
/*  refresh(); */
}

void trefresh_()
{
  refresh();
}

void tmove_(line,col)
int *line, *col;
{
/*  printf(" line, col=%d %d\n",*line,*col); */

   move(*line, *col);
/*   refresh(); */

 }

void tend_()
{
  endwin();
}

void tscreensz_(line, col)
int *line, *col;
{
  *line = LINES;
  *col  = COLS;
}


void tputc_(c)
char *c;
{
  addch(*c);
/*  refresh(); */
}

void tattrreverse_()
{
  attrset(A_REVERSE);
/*  refresh(); */
}

void tattrnormal_()
{
  attrset(A_NORMAL);
/*  refresh(); */
}

void tgetcode_(code,s)
int *code;
char *s;
{
  int c;
  c=getch();
  memset(s,c,1);
  switch(c) { /* Test input code */

    /* hjkl anda arrow keys; move cursor in direction indicated. */
/*    case 'h': */
    case  KEY_LEFT:
      *code = 1;
      break;

/*    case 'j':  */
    case KEY_DOWN:
       *code = 2;
       break;

/*    case 'k': */
    case KEY_UP:
       *code = 3;
       break;

/*    case 'l': */
    case KEY_RIGHT:
       *code = 4;
        break;
    default:
      *code = 0;
  /*    flash(); */
      break;
    }
/*  printf(" code =%d \n",*code) ;*/ 
}



void tscrollok_()
{
  scrollok(stdscr, TRUE);
  echo();
}


void tscrollnotok_()
{
  scrollok(stdscr, FALSE);
  noecho();
}
 

void tscroll_()
{
  scroll(stdscr);
}
    

void tgetstr_(s)
char *s;
{
  getstr(s);
}

void tclear_()
{
  clear();
}
