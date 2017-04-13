#include <sys/time.h>
#include <sys/resource.h>

zcpu_(cputim, iocnt)
/*--------------------------------------------------------------------*/
/*! return current process CPU time and IO count                      */
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
/*  Get user + system execution time in seconds plus I/O count for    */
/*  the current process.                                              */
/*  Output:                                                           */
/*     cputim   R   CPU time in seconds (user + system)               */
/*     iocnt    I   I/O count (only used for accounting)              */
/*  4.2 BSD version (uses "getrusage", has some estimate of IOCNT)    */
/*--------------------------------------------------------------------*/
float *cputim;
int *iocnt;
/*--------------------------------------------------------------------*/
{
   struct rusage rbuf;
/*--------------------------------------------------------------------*/
                                        /* Get resource usage for     */
                                        /* "self".                    */
   getrusage (RUSAGE_SELF, &rbuf);
                                        /* Get CPU time.              */
   *cputim = rbuf.ru_utime.tv_sec + (rbuf.ru_utime.tv_usec / 1000000.0)
      + rbuf.ru_stime.tv_sec +  (rbuf.ru_stime.tv_usec / 1000000.0);
                                        /* Get I/O count (below only  */
                                        /* accounts for real I/O;     */
                                        /* data supplied by the       */
                                        /* cacheing mechanism is      */
                                        /* charged only to the first  */
                                        /* process to read or write   */
                                        /* the data (e.g., asynch     */
                                        /* daemon processes).         */
   *iocnt = (rbuf.ru_inblock + rbuf.ru_oublock);

   return;
}
