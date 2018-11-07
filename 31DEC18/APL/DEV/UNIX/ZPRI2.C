#define Z_pri2__
#include <unistd.h>
#include <string.h>
#include <errno.h>
                                        /* Absolute priorities for    */
                                        /* interactive versus batch   */
                                        /* execution.                 */
#define   INTERACTIVE   0
#define   BATCH         16              /* limit is 19                */

#if __STDC__
   void zpri2_(char oper[4], int *syserr, int *ierr)
#else
   void zpri2_(oper, syserr, ierr)
   char oper[4];
   int *syserr, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! change execution priority of current process                      */
/*# System                                                            */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997                                          */
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
/*  Change the execution priority of the current process.             */
/*  Inputs:                                                           */
/*     oper     R   Operation code ("UPPP" => change to interactive   */
/*                  priority, "DOWN" => change to batch priority)     */
/*  Output:                                                           */
/*     syserr   I   System error code                                 */
/*     ierr     I   Error return code: 0 => no error                  */
/*                     1 => invalid opcode                            */
/*                     2 => illegal request                           */
/*                     3 => other                                     */
/* Generic UNIX version (uses "nice" which will let you lower         */
/* execution priority but never raise it, so successive calls with    */
/* opcode "DOWN" will drive priority increasingly lower).             */
/*--------------------------------------------------------------------*/
{
   int pid, prio, current;
   register int i;
   char op[5];
/*--------------------------------------------------------------------*/
   *syserr = 0;
   *ierr = 0;
                                        /* Form null terminated       */
                                        /* operation code.            */
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Check for valid operation  */
                                        /* and set absolute priority  */
                                        /* request.                   */
   if (strcmp (op, "UPPP") == 0)
      prio = INTERACTIVE;
   else if (strcmp (op, "DOWN") == 0)
      prio = BATCH;
   else {
      *ierr = 1;
      return;
      }
                                        /* Set execution priority.    */
                                        /* "Nice" only allows         */
                                        /* decrements in priority.    */
                                        /* It ignores negative values */
                                        /* and has no error return,   */
                                        /* so assume success.         */
   current = nice (0) ;
   errno = 0;
   if ((nice (prio-current) == -1) && (errno != 0)) {
      *ierr = 2;
      }

   return;
}
