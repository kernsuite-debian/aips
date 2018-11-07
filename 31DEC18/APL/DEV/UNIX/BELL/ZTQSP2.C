#define Z_tqsp2__
#define _XOPEN_SOURCE                      /* Needed for popen/pclose */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <AIPSPROTO.H>

#if __STDC__
   void ztqsp2_(float *doall, int *mslev, int *nproc)
#else
   void ztqsp2_(doall, mslev, nproc)
   float *doall;
   int   *mslev;
   int   *nproc;
#endif
/*--------------------------------------------------------------------*/
/*! display AIPS account or all processes running on the system       */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997-1998                                    */
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
/*  Displays information on the user's terminal regarding AIPS        */
/*  account originated processes or all processes running on the      */
/*  system.                                                           */
/*  Inputs:                                                           */
/*     doall   R   > 0.0 => display all processes                     */
/*     mslev   I   Message level to use calling MSGWRT                */
/*     nproc   I   Max POPS number to scan for                        */
/*  Bell UNIX version - ps to terminal only, mslev ignored            */
/*--------------------------------------------------------------------*/
{
   char locstr[80],
        *ehx = " 123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ",
        foo;
   int bar;
                                        /* All processes.             */
   if (*doall > 0.0) {
      system ("ps -ef");
      }
                                        /* Just AIPS login processes. */
   else {
                                        /* Bell "ps" flavour string */
                                        /* This gets AIPS type procs. */
                                        /* 2        3         4 */
                                        /* 123456789 123456789 12345*/
      sprintf(locstr, "ps -ef | grep '[( ][A-Z][A-Z]*[0-9A-Z][)]* *$'");
                                        /* Get max POPS number ehex'd */
      bar = 1;
      foo = ehx[*nproc];
                                        /* Shorten regexp if needed */
      if (*nproc < 10) {
	 locstr[33] = foo;
	 strcpy(&locstr[34], "][)]* *$'");
      } else {
	 locstr[36] = foo;
      }
      system (locstr);
      }

   return;
}
