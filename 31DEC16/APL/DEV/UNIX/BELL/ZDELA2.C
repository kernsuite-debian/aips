#define Z_delay2__
                                        /* Must not claim POSIX_SOURCE*/
                                        /* types.h would not define   */
                                        /* things needed in mtio.h    */
#ifdef _POSIX_SOURCE
#undef _POSIX_SOURCE
#endif

#include <unistd.h>
#ifdef __sgi
#include <sys/param.h>
#endif
#include <sys/types.h>
#include <sys/time.h>
#include <stdio.h>
#include <signal.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zdela2_(float *secs, int *ierr)
#else
   void zdela2_(secs, ierr)
   float *secs;
   int *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! delay process execution by requested seconds                      */
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
/*  Cause an execution delay for a specified time interval.           */
/*  Inputs:                                                           */
/*     secs   R   Seconds to delay execution (0 < secs < 3600.)       */
/*  Output:                                                           */
/*     ierr   I   Error return code: 0 => no error                    */
/*                   1 => error - bad input parms                     */
/*  System V version, special case for SGI (Clock ticks/sec in HZ)    */
/*--------------------------------------------------------------------*/
{
   unsigned isecs, usecs;
   struct itimerval timer, oldtimer;
   float tmin, lsecs;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   lsecs = *secs;
                                        /* Check for valid request.   */
   if (lsecs > 3600.0 || lsecs <= 0.0) {
      *ierr = 1;
      }
   else {
#ifdef __sgi
      tmin = 1.0 / (float) HZ ;
#endif
#ifdef _SC_CLK_TCK
      tmin = 1.0 / (float) sysconf(_SC_CLK_TCK) ;
#else
      tmin = 1.0 / 60.0 ;
#endif
                                        /* Typical minimum delay is   */
                                        /* about 10 millisecs         */
                                        /* (smaller delays are        */
                                        /* supposed to be trapped     */
                                        /* and bumped up, but why     */
                                        /* rely on the system).       */
      if (lsecs < tmin)
         lsecs = tmin;
                                        /* Set timer interval such    */
                                        /* that timer is disabled     */
                                        /* after requested delay      */
                                        /* expires.                   */
      timer.it_interval.tv_sec = 0;
      timer.it_interval.tv_usec = 0;
                                        /* Set delay desired (tv_usec */
                                        /* must not exceed 1 sec or   */
                                        /* or 1000000 micro-seconds). */
      isecs = lsecs;
      usecs = (lsecs - isecs) * 1000000;
      timer.it_value.tv_sec = isecs;
      timer.it_value.tv_usec = usecs;
                                        /* Repeat in case miss 1st    */
      timer.it_interval.tv_sec = isecs;
      timer.it_interval.tv_usec = usecs;
                                        /* Ignore alarm clock         */
                                        /* signals.                   */
      zignal_ (SIGALRM, SIG_IGN);
                                        /* Set timer.                 */
      if (setitimer (ITIMER_REAL, &timer, &oldtimer) != 0) {
         *ierr = 1;
         perror ("ZDELA2");
         }
                                        /* Wait for alarm.            */
      else {
         isecs = lsecs + 1.0;
         sleep (isecs);
         }
                                        /* turn off interval timer    */
      timer.it_interval.tv_sec = 0;
      timer.it_interval.tv_usec = 0;
      timer.it_value.tv_sec = 0;
      timer.it_value.tv_usec = 0;
      setitimer (ITIMER_REAL, &timer, &oldtimer);
                                        /* Catch alarm clock again    */
      zignal_ (SIGALRM, zabors_);
      }

   return;
}
