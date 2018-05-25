#define Z_prmpt__
#include <errno.h>
#include <signal.h>
#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <NOTST/GNU/history.h>

#define HAVE_STRING_H                   /* Needed inside readline.h   */
#include <NOTST/GNU/readline.h>
#include <AIPSPROTO.H>

                                        /* Local signal-handler.      */
#if __STDC__
   static void zprmpt_sigs(int sig)
#else
   static void zprmpt_sigs(sig)
   int sig;
#endif
{
   extern int kill ();                  /* Arg...some OS's...really!  */

   kill (getpid (), SIGTSTP);
}

#if __STDC__
   void zprmpt_(char ipc[1], char buff[80], int *ierr)
#else
   void zprmpt_(ipc, buff, ierr)
   char ipc[1], buff[80];
   int *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! prompt user and read 80 characters from CRT screen                */
/*# Terminal                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997, 2003                                    */
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
/* ZPRMPT prompts user on CRT screen and reads a line of user input   */
/* via the GNU readline (+history) library.  The input line is placed */
/* in a buffer and either truncated to 80 characters or space-padded  */
/* to 80 characters prior to return.  Non-empty lines are added to a  */
/* history list for possible later access/reuse via the               */
/* history-manipulation key-bindings (a la Bash/Ksh/Emacs--or even    */
/* vi).  If the user is not on an interactive tty, say by giving AIPS */
/* input via a shell-script "here document," then the old             */
/* (non-readline) behavior is used and the input line is not added to */
/* the history.  Use of the readline library can be disabled by       */
/* starting AIPS with the "norl" option.                              */
/*                                                                    */
/* Input:                                                             */
/*    IPC    C*1    prompt character                                  */
/* Output:                                                            */
/*    BUFF   C*80   line of user input.                               */
/*    IERR   I      Error return code: 0 => ok                        */
/*                     1 => read/write error (not implemented/set)    */
/*                                                                    */
/* UNIX GNU-readline version (with no-terminal-reset signal-handler). */
/*                                                                    */
/* JAU 26 Jul 95: Jeff Uphoff, NRAO.  Added GNU readline support.     */
/* JAU 29 Aug 95: Jeff Uphoff, NRAO.  Overhaul of several portions.   */
/* JAU 22 Sep 95: Jeff Uphoff, NRAO.  Added check for userno prompt.  */
/* JAU 25 Jun 97: Jeff Uphoff, NRAO.  Fix prompt bug.                 */
/*--------------------------------------------------------------------*/
{
   char *rlenv = NULL;
   char *temp = NULL;
   register int i, ttyp;
/*--------------------------------------------------------------------*/
   *ierr = 0;

   buff[0] = '\0';                      /* Initialize buffer.         */

                                        /* Check if readline disabled */
   if (!(ttyp = isatty (0)) ||          /* or if not interactive tty. */
         (((rlenv = getenv ("READLINE")) != NULL) &&
         !strcmp (rlenv, "NO"))) {

      char pc = ipc[0];                 /* OK, fall back to old       */
                                        /* prompting and input-       */
                                        /* handling behavior.         */
      if(!(temp = (char *)malloc (81))) {
         perror ("ZPRMPT: malloc failed");
         exit (errno);                  /* This is *very* bad!        */
      }
      putchar (pc);                     /* Display prompt.            */

                                        /* Use old input method.      */
      if (fgets (temp, 81, stdin) == NULL) {
         free (temp);
         temp = NULL;
      }
      ttyp = 0;                         /* We'll fake this for later. */

   } else {                             /* Use GNU readline for input */
      char prompt[2];

      if (ipc[0] == '?')                /* Check if userno. prompt.   */
         rl_bind_key ('\t', NULL);      /* If so, disable TAB's.      */

      zignal_ (SIGTTIN, zprmpt_sigs);   /* All of these signal-       */
      zignal_ (SIGTTOU, zprmpt_sigs);   /* handling shenanigans are   */
                                        /* due to general insanity.   */

      prompt[0] = ipc[0];               /* Ensure prompt sanity.      */
      prompt[1] = '\0';
      temp = readline (prompt);         /* Use readline to prompt     */
                                        /* user and return pointer    */
                                        /* to line of user input.     */
                                        /* readline() does the        */
                                        /* malloc() for us, but we    */
                                        /* must free() the pointer.   */

      zignal_ (SIGTTIN, SIG_DFL);       /* More fun with signals.     */
      zignal_ (SIGTTOU, SIG_DFL);
   }

   if (temp == NULL)                    /* NULL pointer means EOF at  */
      sprintf (buff, "EXIT");           /* *beginning* of user input. */

   else {                               /* Non-NULL means good input. */

      if (*temp && ttyp)                /* If not blank (and          */
         add_history (temp);            /* interactive), put in hist. */
                                        /* unless readline disabled   */
                                        /* (the reason for "faking"   */
                                        /* ttyp = 0 earlier).         */

                                        /* readline() strips the new- */
                                        /* line for us.  We need to   */
                                        /* find the first null and    */
                                        /* then pad the output buffer */
                                        /* with trailing spaces.      */
                                        /* The 80-column limit is     */
                                        /* historical...              */
      for (i = 0; i < 80 && temp[i] != '\0'; i++)
         buff[i] = temp[i];             /* Copy up to first null.     */
      for (i = i; i < 80; i++)
         buff[i] = ' ';                 /* Pad remainder with spaces. */
      if (temp)
         free (temp);                   /* Free the readline pointer. */
   }
   return;
}

/*
 * Local Variables:
 * c-basic-offset: 3
 * comment-column: 40
 * indent-tabs-mode: nil
 * End:
 */
