#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_prpas__
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <termios.h>
#define MAX_PASS_LEN  12

#if __STDC__
   void zprpas_(char *pass, char *buff, int *ierr)
#else
   void zprpas_(pass, buff, ierr)
   char pass[], buff[];
   int *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! prompt user and read 12-character password (invisible) from CRT   */
/*# System                                                            */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997                                         */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;                                                                   */
/*;  This program is free software; you can redistribute it and/or    */
/*;  modify it under the terms of the GNU General Public License as   */
/*;  published by the Free Software Foundation; either version 2 of   */
/*;  the License, or (at your option) any later version.              */
/*;                                                                   */
/*;  This program is distributed in the hope that it will be useful,  */
/*;  but WITHOUT ANY WARRANTY; without even the implied warranty of   */
/*;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the    */
/*;  GNU General Public License for more details.                     */
/*;                                                                   */
/*;  You should have received a copy of the GNU General Public        */
/*;  License along with this program; if not, write to the Free       */
/*;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,     */
/*;  MA 02139, USA.                                                   */
/*;                                                                   */
/*;  Correspondence concerning AIPS should be addressed as follows:   */
/*;         Internet email: aipsmail@nrao.edu.                        */
/*;         Postal address: AIPS Project Office                       */
/*;                         National Radio Astronomy Observatory      */
/*;                         520 Edgemont Road                         */
/*;                         Charlottesville, VA 22903-2475 USA        */
/*--------------------------------------------------------------------*/
/*  Prompts the user on his terminal with the prompt string           */
/*  "Password: " and then reads back an 12-character "password"       */
/*  without anything being visible on the screen.                     */
/*  Inputs:                                                           */
/*     none                                                           */
/*  Outputs:                                                          */
/*     pass   C*12     Password                                       */
/*     buff   I*2(*)   Scratch buffer (if needed)                     */
/*     ierr   I*2      Error return code: 0 => ok                     */
/*                        1 => error                                  */
/*  Posix UNIX version ("getpass" deliberately omitted from Posix)    */
/*  getpass version here was taken from W. Richard Stevens "Advanced  */
/*  Programming in the UNIX Environment"                              */
/*--------------------------------------------------------------------*/
{
   static char     buf[MAX_PASS_LEN+1];
   register int    i, j;
   char            *ptr;
   sigset_t        sig, sigsave;
   struct termios  term, termsave;
   FILE            *fp;
   int             c;
/*--------------------------------------------------------------------*/
   *ierr = 0;

                                           /* getpass subroutine      */
                                           /* get controlling terminal*/
   if ((fp = fopen (ctermid(NULL), "r+")) == NULL) {
      buf[0] = '\0' ;
      *ierr = 1;
      }
   else {
      setbuf (fp, NULL);
                                           /* block SIGINT, SIGTSTP  */
      sigemptyset (&sig);
      sigaddset (&sig, SIGINT);
      sigaddset (&sig, SIGTSTP);
      sigprocmask (SIG_BLOCK, &sig, &sigsave);
                                           /* save terminal state    */
      tcgetattr (fileno(fp), &termsave) ;
                                           /* block echoing          */
      term = termsave;
      term.c_lflag &= ~ (ECHO | ECHOE | ECHOK | ECHONL) ;
      tcsetattr (fileno(fp), TCSAFLUSH, &term);
                                           /* write prompt to term.  */
      fputs ("Password:", fp);
                                           /* read password from term*/
      ptr = buf;
      while ((c = getc (fp)) != EOF && c != '\n') {
         if (ptr < &buf[MAX_PASS_LEN])
            *ptr++ = c;
         }
      *ptr = 0;
      putc ('\n', fp);                     /* we echo newline        */
                                           /* restore terminal state */
      tcsetattr (fileno(fp), TCSAFLUSH, &termsave);
                                           /* restore interupt state */
      sigprocmask (SIG_SETMASK, &sigsave, NULL);
      fclose (fp);
      }
                                           /* copy to output         */
   for (i = 0; i < 12 & buf[i] != '\0'; i++)
       pass[i] = buf[i];
   for (j = i; j < 12; j++)
       pass[j] = ' ';

   return;
}
