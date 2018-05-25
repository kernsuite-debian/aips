#include <stdio.h>
#include <signal.h>

zabor2_(tsknam, isalon, action)
/*--------------------------------------------------------------------*/
/*! establishes or carries out (when appropriate) abort handling      */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
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
/*--------------------------------------------------------------------*/
/*  ZABORT will take one of two actions depending on the value of     */
/*  "action".  If "action" is zero, it establishes abort handling     */
/*  to clean up for programs in the event of an ABORTASK or otherwise */
/*  fatal signal.  Hangup, interrupt (except for AIPS or BATER) and   */
/*  quit are ignored.  If running under the control of a debugger, it */
/*  simply returns (to avoid affecting the debugger signal handling). */
/*  If "action" is non-zero, it will issue an illegal instruction in  */
/*  order to induce the abort handler.  This is mostly to get a       */
/*  traceback for debugging purposes (e.g., when an invalid argument  */
/*  to a subroutine is detected).                                     */
/*                                                                    */
/*  Inputs:                                                           */
/*     tsknam   C*6   Program name                                    */
/*     isalon   I     1 => stand-alone program, else a task           */
/*     action   I     Action indicator code:                          */
/*                       0 => establish abort handling                */
/*                       1 => invoke an illegal instruction           */
/*  Output:                                                           */
/*     none                                                           */
/*  Generic UNIX version (your system may have different/more         */
/*  signals).                                                         */
/*--------------------------------------------------------------------*/
char tsknam[];
int *isalon, *action;
/*--------------------------------------------------------------------*/
{
   extern int (*zabors_)();             /* Signal handler function.   */

   char prog[7];
   register int i;
/*--------------------------------------------------------------------*/
                                        /* Generate an illegal        */
                                        /* instruction?               */
   if (*action == 1)
      abort ();
                                        /* Else establish handler     */

                                        /* Form null terminated       */
                                        /* program name.              */
   for (i = 0; i < 6 && tsknam[i] != ' '; i++)
      prog[i] = tsknam[i];
   prog[i] = '\0';
                                        /* If we are running in debug */
                                        /* mode, don't disturb signal */
                                        /* processing.                */
   if (strcmp (prog, "ZSTRTA") == 0 || strcmp (prog, "AIPS") == 0) {
      if (getenv ("AIPSDBUGR") != NULL)
         return;
      }
   else if (strcmp (prog, "ZSTRTB") == 0 ||
      strcmp (prog, "BATER") == 0) {
      if (getenv ("BATERDBUGR") != NULL)
         return;
      }
   else if (getenv ("DBUGR") != NULL)
      return;
                                        /* Your system may have more  */
                                        /* or different signals.  See */
                                        /* signal(?) and              */
                                        /* "/usr/include/signal.h".   */
   if (*isalon == 1 ||
      getenv ("STANDALONE") != 0) {     /* if AIPS, BATER or          */
                                        /* standalone, catch:         */
      signal (SIGINT, &zabors_);        /* interrupt                  */
      signal (SIGHUP, &zabors_);        /* hangup                     */
      }
   else {                               /* otherwise, ignore:         */
      signal (SIGINT, SIG_IGN);         /* interrupt                  */
      signal (SIGHUP, SIG_IGN);         /* hangup                     */
      }
   signal (SIGQUIT, SIG_IGN);           /* quit (ignore)              */
   signal (SIGILL, &zabors_);           /* illegal instruction        */
   signal (SIGTRAP, &zabors_);          /* trace trap                 */
   signal (SIGIOT, &zabors_);           /* IOT instruction            */
   signal (SIGEMT, &zabors_);           /* EMT instruction            */
/*   signal (SIGFPE, &zabors_) */           /* floating point exception   */
   signal (SIGBUS, &zabors_);           /* bus error                  */
   signal (SIGSEGV, &zabors_);          /* segmentation violation     */
   signal (SIGSYS, &zabors_);           /* bad argument to system     */
                                        /* call                       */
   signal (SIGPIPE, &zabors_);          /* write on a pipe with no    */
                                        /* one to read it             */
   signal (SIGALRM, &zabors_);          /* alarm clock                */
   signal (SIGTERM, &zabors_);          /* software termination       */
                                        /* signal (e.g., "ABORTASK")  */
      signal (SIGCHLD, SIG_IGN);
   return;
}
