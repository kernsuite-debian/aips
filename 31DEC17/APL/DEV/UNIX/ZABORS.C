#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_abors__
#include <sys/types.h>
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <signal.h>
#include <AIPSPROTO.H>

static int num_calls = 0;      /* The number of active calls to
                                  zabors.  Since zabors should not
                                  be called recursively this should
                                  be 0 or 1 */

#if __STDC__
   void zabors_(int isig)
#else
   void zabors_(isig)
   int isig;
#endif
/*--------------------------------------------------------------------*/
/*! does abort handling selected by ZABORT                            */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1998                                          */
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
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   char   msgbuf[80];
   int    i, n3 = 3, n8 = 8;
   pid_t  pid;
/*--------------------------------------------------------------------*/
                                        /* Die immediately if this is */
                                        /* a recursive call           */
   if (num_calls != 0) {
                                        /* Get process id for kill(). */
      pid = getpid ();
                                        /* Reinstate default action   */
                                        /* for illegal instructions.  */
      zignal_ (SIGILL, SIG_DFL);
                                        /* Commit suicide.            */
      kill (pid, SIGILL);
      }
   else {
      num_calls++;
                                        /* Signal number held in low  */
                                        /* order 7 bits.              */
      i = isig & 0177;
                                        /* if AIPS dismount tapes.    */
      zstaip_ ( );
                                        /* Make AIPS accounting entry.*/
      acount_ (&n3);
                                        /* Report signal received.    */
      sprintf (msgbuf, "ZABORS: signal %d received", i);
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
         zignal_ (SIGILL, SIG_DFL);
                                        /* Commit suicide.            */
         kill (pid, SIGILL);
         }

      num_calls--;
      }

   return;
}
