#define Z_delay2__
#include <unistd.h>
#include <time.h>

#if __STDC__
   void zdela2_(float *secs, int *ierr)
#else
   zdela2_(secs, ierr)
   float *secs;
   int *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! delay process execution by requested seconds                      */
/*# System                                                            */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2000                                         */
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
/*                                                                    */
/*  Generic version. Assumes that the POSIX.4 (real-time extensions)  */
/*  nanosleep() interface is available.                               */
/*--------------------------------------------------------------------*/
{
   struct timespec *rqtp, *rmtp;
   struct timespec rqt, rmt;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Check for valid request.   */
   if (*secs > 3600.0 || *secs <= 0.0) {
      *ierr = 1;
      }
   else {
      rqtp = &rqt;
      rmtp = &rmt;
      rqtp -> tv_sec = *secs;
      rqtp -> tv_nsec = (*secs - rqtp -> tv_sec) * 1000000000;
      (void) nanosleep (rqtp, rmtp);
      }

   return;
}
