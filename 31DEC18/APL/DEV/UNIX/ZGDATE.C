#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_gdate__

#include <unistd.h>
#include <sys/types.h>
#include <time.h>

#if __STDC__
   void zgdate_(int idate[3])
#else
   void zgdate_(idate)
   int idate[3];
#endif
/*--------------------------------------------------------------------*/
/*! return the Greenwich date                                         */
/*# System                                                            */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1999                                               */
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
/*  Get the Greenwich date.                                           */
/*  Inputs:                                                           */
/*     none                                                           */
/*  Output:                                                           */
/*     idate   I(3)   Local date where:                               */
/*                      idate(1) = year (19nn)                        */
/*                      idate(2) = month (1-12)                       */
/*                      idate(3) = day (1-31)                         */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   struct tm *lp;
   time_t clock;
/*--------------------------------------------------------------------*/
                                        /* Get time since 00:00:00    */
                                        /* GMT, Jan. 1, 1970 in       */
                                        /* seconds.                   */
   time (&clock);
                                        /* Fill structure with        */
                                        /* broken-down time.          */
   lp = gmtime (&clock);
   if (lp==0) lp = localtime (&clock);
   if (lp == 0) {
      idate[0] = 2099;
      idate[1] = 2;
      idate[2] = 31;
      }
   else {
                                        /* Get year and add 1900.     */
      idate[0] = lp->tm_year;
      idate[0] = idate[0] + 1900;
                                        /* Get month (1-relative).    */
      idate[1] = lp->tm_mon;
      idate[1] = idate[1] + 1;
                                       /* Get day of month.          */
      idate[2] = lp->tm_mday;
      }
                                        /* Exit.                      */
   return;
}
