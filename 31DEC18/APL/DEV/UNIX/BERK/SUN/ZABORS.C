#include <stdio.h>
#include <signal.h>

static int num_calls = 0;      /* The number of active calls to
                                  zabors.  Since zabors should not
                                  be called recursively this should
                                  be 0 or 1 */
#ifndef FPE_FPA_ERROR
#define FPE_FPA_ERROR     0x00
#endif

zabors_(isig, code, scp)
/*--------------------------------------------------------------------*/
/*! does abort handling selected by ZABORT                            */
/*# System                                                            */
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
/*  Abort handler routine.  Error conditions that are being trapped   */
/*  are defined in ZABORT.                                            */
/*  Inputs:                                                           */
/*     isig   I   Signal number (held in the low order 7 bits).       */
/*     code   I   Parameter that futher defines the type of hardware  */
/*                exception that has occurred                         */
/*     scp    P   Pointer to structure used to restore the context    */
/*                from before the signal                              */
/*  SUN version - traps and ignores FPA induced FPEs.                 */
/*--------------------------------------------------------------------*/
int isig, code;
struct sigcontext *scp;
/*--------------------------------------------------------------------*/
{
   extern char *sys_siglist[];
   char msgbuf[80];
   int i, n3 = 3, n8 = 8, pid;
/*--------------------------------------------------------------------*/
                                        /* Die immediately if this is */
                                        /* a recursive call           */
   if (num_calls != 0) {
                                        /* Get process id for kill(). */
      pid = getpid ();
                                        /* Reinstate default action   */
                                        /* for illegal instructions.  */
      signal (SIGILL, SIG_DFL);
                                        /* Commit suicide.            */
      kill (pid, SIGILL);
      }
   else {
   if (num_calls == 1) return;
      num_calls++;
                                        /* Special handling of FPA    */
                                        /* floating point exceptions. */
#ifndef sparc
      if (isig == SIGFPE && code == FPE_FPA_ERROR) {
         sigfpe_handler (isig, code, scp);
         }
      else {
#endif
                                        /* Signal number held in low  */
                                        /* order 7 bits.              */
         i = isig & 0177;
                                        /* if AIPS dismount tapes.    */
         zstaip_ ( );
                                        /* Make AIPS accounting entry.*/
         acount_ (&n3);
                                        /* Report signal received.    */
         sprintf (msgbuf, "ZABOR2: %s", sys_siglist[i]);
         zmsgwr_ (msgbuf, &n8);
                                        /* Issue abort message.       */
         sprintf (msgbuf, "ABORT!");
         zmsgwr_ (msgbuf, &n8);
                                        /* clear task id file         */
         zdie_ ( ) ;
                                        /* Just exit on some errors.  */
         i = isig & 0177;
         if (i < 4 || i == 15) {
            exit(1);
            }
                                        /* On others, abort.          */
         else {
                                        /* Get process id for kill(). */
            pid = getpid ();
                                        /* Reinstate default action   */
                                        /* for illegal instructions.  */
            signal (SIGILL, SIG_DFL);
                                        /* Commit suicide.            */
            kill (pid, SIGILL);
            }
#ifndef sparc
         }
#endif

      num_calls--;
      }

   return;
}
