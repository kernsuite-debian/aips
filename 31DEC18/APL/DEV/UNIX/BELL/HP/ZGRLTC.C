#define Z_grltc__
#include <ctype.h>
#include <errno.h>
#include <malloc.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define HAVE_STRING_H
#include <NOTST/GNU/readline.h>
#include <AIPSPROTO.H>
#include <NOTST/GNU/TABS.H>

static char **te_list = (char **)NULL;
static char *zgrltc_cm ();
static char *zgrltc_dup ();
static char **zgrltc_acf ();
static int quoted;
static int tasks_ok;


#if __STDC__
   void zgrltc_ (int *ierr)
#else
   void zgrltc_ (ierr)
   int *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! inizializes completion list for TAB-key completion of commands    */
/*# Terminal                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1996                                         */
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
/* ZGRLTC initializes the list(s) used for TAB-completion of commands */
/* and sets appropriate "hooks" in the GNU readline library for its   */
/* use.  If AIPS is started with the "norl" option, this feature is   */
/* disabled.                                                          */
/*                                                                    */
/* Input:                                                             */
/*    None.                                                           */
/* Output:                                                            */
/*    IERR   I      Error return code: 0 => ok                        */
/*                     <other> => error or disabled                   */
/*                                                                    */
/* Generic UNIX version for OS's implementing GNU's libreadline       */
/*                                                                    */
/* JAU 21 Sep 95: Jeff Uphoff, NRAO.                                  */
/* JAU 25 Sep 95: Jeff Uphoff, NRAO.  Made smarter about quoted arg's */
/* JAU 23 Feb 96: Jeff Uphoff, NRAO.  strdup() cleanup, dynamic list. */
/*--------------------------------------------------------------------*/
{
   char buf[LISTBUFLEN];
   char file[MAXAPATH];
   char *envp = NULL;
   int i = 0;
   FILE *fp;
/*--------------------------------------------------------------------*/
   *ierr = 1;                           /* Assume abject failure.     */

                                        /* Check if readline disabled */
   if (((envp = getenv ("READLINE")) != NULL) &&
       !strcmp (envp, "NO"))
      return;                           /* If so, skip the rest.      */

                                        /* Check for help file dir.   */
   if ((envp = getenv ("AIPS_VERSION")) == NULL)
      return;                           /* None defined?  Give up...  */

                                        /* Build filename for list.   */
   sprintf (file, "%s/HELP/%s", envp, HLPLISTFILE);
                                        /* Open completion list file. */
   if ((fp = fopen (file, "r")) == NULL)
      return;                           /* Argh!  Run to mama...      */

                                        /* Do initial malloc--not all */
                                        /* OS's need this, but some   */
                                        /* do for the later reallocs. */
   if (!(te_list = (char **)malloc (sizeof (char *)))) {
      perror ("ZGRLTC: malloc failed");
      exit (errno);
   }
                                        /* Read file, build list.     */
   while (fgets (buf, LISTBUFLEN, fp) != NULL) {
      if (!(te_list = (char **)realloc (te_list,
                                        sizeof (char *) * (i + 1)))) {
         perror ("ZGRLTC: realloc failed");
         exit (errno);
      }
      te_list[i++] = zgrltc_dup (strtok (buf, "\n"));
   }
   fclose (fp);
                                        /* Terminate list. Thanx HP.  */
   if (!(te_list = (char **)realloc (te_list,
                                     sizeof (char *) * (i + 1)))) {
      perror ("ZGRLTC: terminating realloc failed");
      exit (errno);
   }
   te_list[i] = 0;
                                        /* Set completion function.   */
   rl_attempted_completion_function = (CPPFunction *)zgrltc_acf;
                                        /* Block filename completions */
   rl_completion_entry_function = (Function *)zgrltc_cm;
   rl_completer_quote_characters = "'"; /* Quoted strings are special */
   rl_bind_key ('\t', rl_complete);     /* Set default completion key */
   rl_read_init_file (NULL);            /* For key-binding overrides. */
   *ierr = 0;                           /* All is well.               */
}
/*--------------------------------------------------------------------*/


#if __STDC__
   static char **zgrltc_acf (char *user_txt, int start, int end)
#else
   static char **zgrltc_acf (user_txt, start, end)
   char *user_txt;
   int start, end;
#endif
/*--------------------------------------------------------------------*/
/*! Performs TAB-key completion of commands                           */
/*# Terminal                                                          */
/*--------------------------------------------------------------------*/
/* ZGRLTC_ACF is used *internally* to perform TAB-key completions.    */
/*                                                                    */
/* Input and output are determined by the GNU readline library's      */
/* internal functionality.                                            */
/*--------------------------------------------------------------------*/
{
   int i;
/*--------------------------------------------------------------------*/
   tasks_ok = 0;                        /* Assume first arg.          */

   for (i = 0; i < start; i++)          /* Check for non-whitespace.  */
      if (rl_line_buffer[i] != ' ' && rl_line_buffer[i] != '\'') {
         tasks_ok = 1;                  /* Found some, tasks now OK.  */
         break;
      }
                                        /* Check for quoted arg.      */
   quoted = start && rl_line_buffer[start - 1] == '\'' ? 1 : 0;
                                        /* Call GNU-RL and start work */
   return (completion_matches (user_txt, zgrltc_cm));
}
/*--------------------------------------------------------------------*/


#if __STDC__
   static char *zgrltc_cm (char *user_txt, int state)
#else
   static char *zgrltc_cm (user_txt, state)
   char *user_txt;
   int state;
#endif
/*--------------------------------------------------------------------*/
/*! Generates matches for TAB-key completion of commands              */
/*# Terminal                                                          */
/*--------------------------------------------------------------------*/
/* ZGRLTC_CM is used *internally* to generate matches for TAB-key     */
/* completions.                                                       */
/*                                                                    */
/* Input and output are determined by the GNU readline library's      */
/* internal functionality.                                            */
/*--------------------------------------------------------------------*/
{
   static int len, list_index;          /* Sticky across calls.       */
   int i;
/*--------------------------------------------------------------------*/
   if (!state) {                        /* Check for new completion.  */
      list_index = 0;                   /* Yes?  Reset list index.    */
      len = strlen (user_txt);          /* Count our pennies.         */

      if (!quoted)                      /* Check for quoted arg.      */
         for (i = 0; i < len; i++)      /* No?  Then lowercase it.    */
            user_txt[i] = tolower (user_txt[i]);
   }

   while (te_list[list_index]) {        /* Walk the list.             */

                                        /* Tasks are prefixed with    */
                                        /* '-' in the completion list */
                                        /* found in HLPLISTFILE.      */

                                        /* Check if task.             */
      if (te_list[list_index][0] == '-') {

                                        /* Now check for match and    */
                                        /* whether tasks are allowed. */
         if (!strncmp (&te_list[list_index][1], user_txt, len) &&
             tasks_ok)
            return (zgrltc_dup (&te_list[list_index++][1]));

      } else {                          /* Not a task.                */
         if (!strncmp (te_list[list_index], user_txt, len))
            return (zgrltc_dup (te_list[list_index++]));
      }
      ++list_index;
   }
   return ((char *)NULL);               /* No matches, or end of list */
}
/*--------------------------------------------------------------------*/


#if __STDC__
   static char *zgrltc_dup (const char *todup)
#else
   static char *zgrltc_dup (todup)
   char *todup;
#endif
/*--------------------------------------------------------------------*/
/*! Does string duplication for TAB-key completion of commands        */
/*# Terminal                                                          */
/*--------------------------------------------------------------------*/
/* ZGRLTC_DUP is used *internally* to emulate the SVID/BSD strdup(3)  */
/* library function.                                                  */
/*                                                                    */
/* Input and output are determined by the GNU readline library's      */
/* internal functionality requrements.                                */
/*--------------------------------------------------------------------*/
{
   char *duped;
/*--------------------------------------------------------------------*/
   if (!(duped = (char *)malloc (strlen (todup) + 1))) {
      perror ("ZGRLTC: zgrltc_dup() malloc failed");
      exit (errno);                     /* Cancel Christmas!          */
   }
   return (strcpy (duped, todup));
}
/*--------------------------------------------------------------------*/

/*
 * Local Variables:
 * c-basic-offset: 3
 * comment-column: 40
 * indent-tabs-mode: nil
 * End:
 */
