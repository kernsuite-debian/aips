#include <sys/time.h>
#include <signal.h>

zdela2_(secs, ierr)
/*--------------------------------------------------------------------*/
/*! delay process execution by requested seconds                      */
/*# System                                                            */
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
/*  Cause an execution delay for a specified time interval.           */
/*  Inputs:                                                           */
/*     secs   R   Seconds to delay execution (0 < secs < 3600.)       */
/*  Output:                                                           */
/*     ierr   I   Error return code: 0 => no error                    */
/*                   1 => error - bad input parms                     */
/*  AIX version (uses "usleep" to achieve 10 millisecond              */
/*  resolution).                                                      */
/*--------------------------------------------------------------------*/
float *secs;
int *ierr;
/*--------------------------------------------------------------------*/
{
   int isecs, usecs;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Check for valid request.   */
   if (*secs > 3600.0 || *secs <= 0.0) {
      *ierr = 1;
      }
   else {
                                        /* Typical minimum delay is   */
                                        /* about 10 millisecs.        */
      if (*secs < 0.01)
         *secs = 0.01;
                                        /* Set delay desired in       */
                                        /* micro-seconds.             */
      isecs = *secs;
      usecs = (*secs - isecs) * 1000000;
      usecs = (usecs < 0 ? 0 : usecs) ; /* For Safety                 */
      if ( usleep( (unsigned int) usecs ) != 0 ) {
         *ierr = 1;
         perror ("ZDELA2");
         }

      else {
         sleep (isecs);
         }
      }

   return;
}
