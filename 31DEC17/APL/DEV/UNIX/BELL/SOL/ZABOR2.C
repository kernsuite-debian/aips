#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_abor2__
#include <ieeefp.h>
#include <unistd.h>
#include <stdio.h>
#include <signal.h>
#include <stdlib.h>
#include <string.h>
/*                                          For SIGFPE_DEFAULT        */
#include <floatingpoint.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zabor2_(char tsknam[], int *isalon, int *action)
#else
   void zabor2_(tsknam, isalon, action)
   char tsknam[];
   int *isalon, *action;
#endif
/*--------------------------------------------------------------------*/
/*! establishes or carries out (when appropriate) abort handling      */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1998, 2003                                    */
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
/*                                                                    */
/*  Solaris 2.x version: identical to the generic UNIX version except */
/*  that it sets traps for invalid floating-point operations,         */
/*  floating-point overflow and floating-point division by zero.      */
/*--------------------------------------------------------------------*/
{
   char prog[7];
   register int i;
   fp_except mask;                      /* Floating-point exception   */
                                        /* mask.                      */
/*--------------------------------------------------------------------*/
                                        /* Abort?                     */
   if (*action == 1)
      abort ();
                                        /* Else establish handler     */

                                        /* Form null terminated       */
                                        /* program name.              */
   strncpy (prog, tsknam, 7);
                                        /* Configure the floating-    */
                                        /* point environment.         */
   mask = fpgetmask ();
   fpsetmask (mask | FP_X_INV | FP_X_OFL | FP_X_DZ);
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
                                        /* ppid > 1 => dbgr           */
   else if ((getenv ("DBUGR") != NULL) && (getppid () > 1))
      return;
                                        /* Your system may have more  */
                                        /* or different signals.  See */
                                        /* signal(3) and              */
                                        /* "/usr/include/signal.h".   */
   if (*isalon == 1 ||
      getenv ("STANDALONE") != 0) {     /* if AIPS, BATER or          */
                                        /* standalone, catch:         */
      zignal_ (SIGINT, zabors_);        /* interrupt                  */
      zignal_ (SIGHUP, zabors_);        /* hangup                     */
      }
   else {                               /* otherwise, ignore:         */
      zignal_ (SIGINT, SIG_IGN);        /* interrupt                  */
      zignal_ (SIGHUP, SIG_IGN);        /* hangup                     */
      }
   zignal_ (SIGQUIT, SIG_IGN);          /* quit (ignore)              */
   zignal_ (SIGILL, zabors_);           /* illegal instruction        */
#ifdef SIGTRAP
   zignal_ (SIGTRAP, zabors_);          /* trace trap                 */
#endif
#ifdef SIGABRT
   zignal_ (SIGABRT, zabors_);          /* abort (IOT) instruction    */
#else
   zignal_ (SIGIOT, zabors_);           /* abort (IOT) instruction    */
#endif
#ifdef SIGEMT
   zignal_ (SIGEMT, zabors_);           /* EMT instruction            */
#endif
   zignal_ (SIGFPE, zabors_);           /* floating-point exception   */
   zignal_ (SIGKILL, SIG_DFL);          /* kill: can't catch or ignore*/
#ifdef SIGBUS
   zignal_ (SIGBUS, zabors_);           /* bus error                  */
#endif
   zignal_ (SIGSEGV, zabors_);          /* segmentation violation     */
#ifdef SIGSYS
   zignal_ (SIGSYS, zabors_);           /* bad argument to system call*/
#endif
   zignal_ (SIGPIPE, zabors_);          /* write on a pipe with no one*/
                                        /*    to read it              */
   zignal_ (SIGTERM, zabors_);          /* software termination       */
                                        /*    (e.g., "ABORTASK")      */
   zignal_ (SIGUSR1, zabors_);          /* user defined signal 1      */
   zignal_ (SIGUSR2, zabors_);          /* user defined signal 2      */

   zignal_ (SIGALRM, zabors_);          /* alarm clock                */

   return;
}
void ieee_retrospective_()
{
   ieee_handler ("set", "common", SIGFPE_DEFAULT);
   ieee_flags ("clearall", "", "", "");
}
