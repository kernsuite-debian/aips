#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_cpu__
#include <unistd.h>
#include <sys/types.h>
#include <sys/times.h>

#if __STDC__
   void zcpu_(float *cputim, int *iocnt)
#else
   void zcpu_(cputim, iocnt)
   float *cputim;
   int *iocnt;
#endif
/*--------------------------------------------------------------------*/
/*! return current process CPU time and IO count                      */
/*# System                                                            */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997-1998                                         */
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
/*  Get user + system execution time in seconds plus I/O count for    */
/*  the current process.                                              */
/*  Output:                                                           */
/*     cputim   R   CPU time in seconds (user + system)               */
/*     iocnt    I   I/O count (only used for accounting)              */
/*  Generic Posix UNIX version - IOCNT unavailable.                   */
/*  System ticks/second requested at run time                         */
/*--------------------------------------------------------------------*/
{
   struct tms buf;
/*--------------------------------------------------------------------*/
                                        /* Get CPU time (be careful   */
                                        /* with units!).              */
   times (&buf);
#ifdef _SC_CLK_TCK
   *cputim = (buf.tms_utime + buf.tms_stime) /
      (float) sysconf(_SC_CLK_TCK) ;
#else
   *cputim = (buf.tms_utime + buf.tms_stime) / 60.0 ;
#endif
                                        /* No way to get I/O count.   */
   *iocnt = 0;
                                        /* Exit.                      */
   return;
}
