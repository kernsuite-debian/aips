#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <AIPSPROTO.H>
#if __STDC__
   void zdie2_(char cpname[])
#else
   void zdie2_(cpname)
   char cpname[];
#endif
/*--------------------------------------------------------------------*/
/*! closes a task at system level: delete /tmp/<task><npops>.<pid>    */
/*# Z2 System                                                         */
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
/*   closes a task at system level: delete /tmp/<task><npops>.<pid>   */
/*   Inputs:                                                          */
/*      CPNAME   C(6)   Actual task name including pops number        */
/*--------------------------------------------------------------------*/
{
   int i;
   char procname[7];                   /* desired process name.       */
   char lockname[24]             ;     /* Name of lock file           */
   pid_t mypid;                        /* PID of this process         */
/*--------------------------------------------------------------------*/
                                       /* copy process name           */
   for (i = 0; i != 6 && cpname[i] != ' '; i++) {
      procname[i] = cpname[i];
      }
   procname[i] = '\0';
                                       /* get process id number       */
   mypid = getpid ();
                                       /* make file name              */
   sprintf (lockname, "/tmp/%s.%d", procname, mypid);
                                       /* delete file                 */
   unlink (lockname);
                                       /* copy process name           */
}
