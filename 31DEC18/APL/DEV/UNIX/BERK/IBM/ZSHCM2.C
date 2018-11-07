#include <errno.h>
/* #include <stdio.h> */
#define MAXCOM 1024
zshcm2_(lcom, commnd, ltask, outdev, ierr)
/*--------------------------------------------------------------------*/
/*! execute a job-control level command string                        */
/*# Z3 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 2004-2005                                    */
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
/*  Execute a "shell" (DCL or sh) command string below present task.  */
/*  Inputs:                                                           */
/*     lcom     I       length of commnd                              */
/*     commnd   C*(*)   The command line string                       */
/*     ltask    I       length of outdev                              */
/*     outdev   C*(*)   file/device for standard output/error         */
/*  Output:                                                           */
/*     ierr     I       error return: 0 means okay                    */
/*                         1 input command too long                   */
/*                         2 some system error occurred               */
/*  IBM version - OUTDEV argument currently ignored, uses fork rather */
/*  than system since that doesn't work on IBMs                       */
/*--------------------------------------------------------------------*/
int *lcom, *ltask, *ierr;
char commnd[], outdev[];
/*--------------------------------------------------------------------*/
{
   register int i, j;
   char com[MAXCOM];
/*   pid_t  pid0, pid1;/
/*--------------------------------------------------------------------*/
   *ierr = 0;
   if (*lcom >= MAXCOM)
      *ierr = 1;
                                        /*  make null-terminated      */
   else {
      for (i = 0; i < MAXCOM && i < *lcom; i++) {
         com[i] = commnd[i];
         if (com[i] != ' ') j = i;
         }
      com[j+1] = '\0';
                                        /*  execute the command       */
                                        /* We are the child           */
/*      if ((pid0 = fork()) == 0) {
         execl ("/bin/sh", "sh", "-c", com, (char *) 0);
         fprintf (stderr, "ZSHCMD: Should never get here!\n");
         *ierr = 2;
         } */
                                        /* We're the parent          */
/*      else {
         if (errno == 0) {
            pid1 = wait ((void *) 0);
            }
         } */
    if ((i = system(com)) != 0) {
         *ierr = 2;
         if ((i == -1) && (errno == 10)) *ierr = 0;
         }
      }

   return;
}
