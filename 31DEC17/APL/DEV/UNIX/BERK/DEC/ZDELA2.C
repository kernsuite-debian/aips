/* ZDELA2.C */
/*--------------------------------------------------------------------*/
/*! delay process execution                                           */
/*# System Z-2                                                        */
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
/*  Delay execution for a specified time interval.                    */
/*                                                                    */
/*  Input:                                                            */
/*    secs        R     Time to delay execution, in seconds           */
/*                      (0.0 < secs <= 3600.0)                        */
/*                                                                    */
/*  Output:                                                           */
/*    ierr        I     Error return code: 0 if no errors are         */
/*                      detected; 1 if secs is out of range, 2 if a   */
/*                      system error has been detected.               */
/*                                                                    */
/*  POSIX/BSD UNIX version -- uses the Berkeley interval timer to     */
/*  achieve a resolution greater than 1 second.                       */
/*--------------------------------------------------------------------*/

/*
 * Ensure that the necessary feature test macros are defined.
 * _POSIX_SOURCE must be defined in order to make the definitions of
 * the signal handling functions available.  Other feature test macros
 * may have to be defined in order to make setitimer() available.
 */
#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif

#ifndef _CONVEX_SOURCE
#define _CONVEX_SOURCE
#endif

/*
 * Other machine or O/S specific feature test macros can be added here.
 * It should be harmless to define feature test macros that are not used
 * by the machine that this routine is compiled on.
 */

#include <sys/time.h>
#include <assert.h>
#include <signal.h>
#include <stdio.h>

/*
 * Declaration of the SIGALRM handler.
 */
static void sigalrm_handler();

/*
 * Wait for a specified amount of time.
 */
void
zdela2_ (secs, ierr)
/*--------------------------------------------------------------------*/
float *secs;  /* The time to wait, in seconds (0.0 < secs <= 3600.0) */
int   *ierr;  /* The error return code */
/*--------------------------------------------------------------------*/
{
   struct itimerval timer;    /* Interval to wait */
   struct itimerval oldtimer; /* Old contents of interval timer */

   void (*saved_handler)(); /* A pointer to the SIGALRM handler as it
                               was on entry to this routine. */

   sigset_t block_mask; /* A signal mask with only SIGALRM blocked */
   sigset_t alarm_mask; /* A signal mask with all signals except
                           SIGALRM blocked */
/*--------------------------------------------------------------------*/

   if ( (0.0 < *secs) && (*secs <= 3600.0) ) {

      /*
       * Convert floating point time to a Berkeley timer value:
       */
      timer.it_interval.tv_sec = 0;
      timer.it_interval.tv_usec = 0;
      timer.it_value.tv_sec = (int) *secs;
      timer.it_value.tv_usec = (int) (1.0e6 * (*secs -
                                               timer.it_value.tv_sec));

      /*
       * It is necessary to make sure that a handler has been defined
       * for SIGALRM otherwise sigsuspend() may never wake up.  The
       * ANSI standard C function signal() is used instead of the the
       * more flexible POSIX.1 sigaction() because some of the fields
       * in the sa_handler structure are undefined since AIPS installs
       * error handlers using signal() in ZABOR2.C (see POSIX.1 p65).
       */
      if ( (int)(saved_handler = signal (SIGALRM, sigalrm_handler)) != -1 ) {

         /*
          * A handler has now been installed.
          */

         /*
          * Setting the timer and waiting for it to expire is a critical
          * region of code:  the SIGALRM from the interval timer must be
          * blocked until sigsuspend() is called.
          */

          sigemptyset (&block_mask);
          sigaddset (&block_mask, SIGALRM);
          if ( sigprocmask (SIG_BLOCK, &block_mask, NULL) == 0 ) {

             /*
              * SIGALRM is blocked.
              */

             sigfillset (&alarm_mask);
             sigdelset (&alarm_mask, SIGALRM);

             if ( setitimer (ITIMER_REAL, &timer, &oldtimer)  == 0 ) {

                /*
                 * Timer is running.
                 */

                /*
                 * Suspend process until SIGALRM is received:
                 */
                (void) sigsuspend (&alarm_mask);

                /*
                 * Unblock SIGALRM:
                 */
                if ( sigprocmask (SIG_UNBLOCK, &block_mask, NULL) == 0) {

                   /*
                    * SIGALRM is unblocked.
                    */

                   /*
                    * Restore SIGALRM handler:
                    */
                   if ( (int)signal (SIGALRM, saved_handler) != -1 ) {

                      /*
                       * SIGALRM handler is restored.
                       */

                      *ierr = 0;
                   } else {

                      /*
                       * Failed to restore SIGALRM handler.
                       */

                      fprintf (stderr,
                               "ZDELA2: failed to restore SIGALRM handler\n");
                      perror ("ZDELA2");
                      *ierr = 2;
                   }
                } else {

                   /*
                    * Failed to unblock SIGALRM.
                    */

                   fprintf (stderr, "ZDELA2: failed to unblock SIGALRM\n");
                   perror ("ZDELA2");
                   *ierr = 2;
                }
             } else {

                /*
                 * Failed to set interval timer.
                 */

                fprintf (stderr, "ZDELA2: failed to set interval timer\n");
                perror ("ZDELA2");
                *ierr = 2;
             }
          } else {

             /*
              * Failed to block SIGALRM.
              */

             fprintf (stderr, "ZDELA2: failed to block SIGALRM\n");
             perror ("ZDELA2");
             *ierr = 2;
          }
       } else {

          /*
           * Failed to install SIGALRM handler.
           */

          fprintf (stderr, "ZDELA2: failed to install SIGALRM handler\n");
          perror ("ZDELA2");
          *ierr = 2;
      }
   } else {

      /*
       * secs is out of range.
       */

      *ierr = 1;

   }
}

/*
 * Dummy SIGALRM handler.
 */
static void
sigalrm_handler (sig)
int sig; /* Signal type (should be SIGALRM) */
{
   assert (sig == SIGALRM);
}



