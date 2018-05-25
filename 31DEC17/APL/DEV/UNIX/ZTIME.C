#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_time__

#include <unistd.h>
#include <sys/types.h>
#include <time.h>

#if __STDC__
   void ztime_(int itime[3])
#else
   void ztime_(itime)
   int itime[3];
#endif
/*--------------------------------------------------------------------*/
/*! return the local time of day                                      */
/*# Service                                                           */
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
/*  Get the local time.                                               */
/*  Output:                                                           */
/*     itime   I*2(3)   Local time where: itime(1) = hour (0-24)      */
/*                                        itime(2) = minute (0-60)    */
/*                                        itime(3) = second (0-60)    */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   struct tm *lp;
   time_t clock;
/*--------------------------------------------------------------------*/
                                        /* Get time since 00:00:00    */
                                        /* GMT, Jan. 1, 1970 in       */
                                        /* seconds.                   */
   time(&clock);
                                        /* Fill structure with        */
                                        /* broken-down time.          */
   lp = localtime(&clock);
                                        /* Get hour.                  */
   itime[0] = lp->tm_hour;
                                        /* Get minute.                */
   itime[1] = lp->tm_min;
                                        /* Get second.                */
   itime[2] = lp->tm_sec;

   return;
}
