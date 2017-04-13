#define Z_strtp__
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zstrtp_(int *mypops, int *myprio, int *ierr)
#else
   void zstrtp_(mypops, myprio, ierr)
   int *mypops, *myprio, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! set the process execution priority based on POPS number           */
/*# System                                                            */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2000                                   */
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
/*  Set the execution priority of this process based on its POPS #.   */
/*  Inputs:                                                           */
/*     mypops   I   POPS # of current process                         */
/*  Output:                                                           */
/*     myprio   I   Execution priority (default = 0)                  */
/*     ierr     I   Error return code: 0 => no error                  */
/*                     1 => error                                     */
/*  Generic UNIX version (uses "nice").                               */
/*  Currently used only by Convex-local ZSTRTA                        */
/*--------------------------------------------------------------------*/
{
#if __STDC__
   pid_t pid;
   uid_t uid;
#else
   int pid, uid ;
#endif
   int  prio, llen, xlen, xlnb, jerr;
   char lognam[MAXPNAME], xlated[MAXAPATH];
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Default priority to 0.     */
   prio = 0;
   *myprio = 0;
                                        /* Form the name of the       */
                                        /* priority logical (i.e.,    */
                                        /* environment variable) for  */
                                        /* "*mypops" and attempt to   */
                                        /* translate it.              */
   llen = MAXPNAME;
   sprintf (lognam, "PRIO%x", *mypops);
   xlen = MAXAPATH;
   ztrlo2_ (&llen, lognam, &xlen, xlated, &xlnb, &jerr);
                                        /* If defined, extract the    */
                                        /* priority from the          */
                                        /* translated character value */
                                        /* as an integer value.       */
   if (jerr == 0) sscanf (xlated, "%d", &prio);
                                        /* Get current process id.    */
   if ((pid = getpid ()) == -1) {
      *ierr = 1;
      perror ("ZSTRTP: getpid");
      }
                                        /* Set execution priority.    */
                                        /* NOTE: Increasing priority  */
                                        /* requires super-user        */
                                        /* priviledge.                */
   else if (nice (prio) == -1) {
      *ierr = 1;
      perror("ZSTRTP: nice");
      }
   else {
       *myprio = prio;
      }
                                        /* Get current, real user id. */
   if ((uid = getuid ()) == -1) {
      perror ("ZSTRTP: getuid");
                                        /* Self destruct.             */
      abort ();
      }
                                        /* Set the effective user id  */
                                        /* to the real user id.       */
   else if (seteuid (uid) == -1) {
      perror ("ZSTRTP: seteuid");
                                        /* Self destruct.             */
      abort ();
      }

   return;
}
