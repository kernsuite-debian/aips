#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_tact2__
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <dirent.h>
#include <signal.h>
#include <sys/stat.h>

#if __STDC__
   void ztact2_(int *len, char name[6], pid_t *pid, int *ierr)
#else
   ztact2_(len, name, pid, ierr)
   char name[6];
   int *len, *pid, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! inquires if a task is currently active on the local computer      */
/*# Z2 System                                                         */
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
/*  Tests to see if a process exists.                                 */
/*  Inputs:                                                           */
/*     len    I      Number of characters used in name                */
/*     name   C*6    Actual task name.                                */
/*  In/out:                                                           */
/*     pid    I(3)   Process "ID" code                                */
/*                   In:  PID(1-3) process #  0 => unknown            */
/*                   Out: PID(1-3) the ID determined here from NAME   */
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => ok, task active          */
/*                      1 => task not active                          */
/*                      2 => invalid task name.                       */
/*                      3 => other                                    */
/* Bell UNIX version.                                                 */
/*--------------------------------------------------------------------*/
{
   char pname[7], cpid[7], filename[14], pathname[20];
   pid_t procid;
   gid_t gid;
   DIR *dfd;
   struct dirent *dirp;
   struct stat stbuf;
   register int i, j;
/*--------------------------------------------------------------------*/
                                        /* Assume task is not active. */
   *ierr = 1;
                                        /* Form null terminated       */
                                        /* process name.              */
   for (i = 0; i < 6 && i < *len && name[i] != ' '; i++)
      pname[i] = name[i];
   pname[i] = '\0';
                                        /* Determine via process id   */
   if (*pid != 0) {
      if ((kill(*pid,0) == 0) || (errno == EPERM)) {
         *ierr = 0;
         }
                                        /* Dead process lockfile.     */
                                        /* Remove.                    */
      else {
         sprintf(pathname,"/tmp/%s.%d",pname,*pid);
         unlink(pathname);
         }
      }
                                        /* Determine pid from process */
                                        /* name as provided. Look for */
                                        /* regular files in /tmp with */
                                        /* names of the form          */
                                        /* 'pname.pid' MX1.12345)     */
   else {
                                        /* Get "our" group id.        */
      gid = getgid();
                                        /* Open directory /tmp.       */
      if ((dfd = opendir ("/tmp")) != NULL) {
                                        /* Search for filename of the */
                                        /* form 'pname.pid'.          */
         while ((dirp = readdir (dfd)) != NULL) {
                                        /* Directory slot not in use. */
            if (dirp->d_fileno == 0)    /* use fileno, not std ino    */
               continue;
                                        /* Compare.                   */
            for (i=0; i < 6 && pname[i] != '\0'; i++) {
               if (dirp->d_name[i] != pname[i]) {
                                        /* Mismatch                   */
                  goto skip;
                  }
               }
                                        /* Get full filename.         */
            for (i = 0; i < 14 && dirp->d_name[i] != '\0'; i++) {
               filename[i] = dirp->d_name[i];
               if (filename[i] == '.') j = i;
               }
            filename[i] = '\0';
            sprintf (pathname, "/tmp/%s", filename);
                                        /* Get status of pathname.    */
                                        /* Is it a regular file?      */
                                        /* Does it belong to "us"?    */
            if (stat(pathname, &stbuf) == -1) {
               printf("ZTACT2: stat on %s failed!\n",pathname);
               goto skip;
               }
/*
                                        ??? stbuf.st_gid is always
                                        zero (at least on the Convex)
            else if (stbuf.st_gid != gid) {
               printf("ZTACT2: stbuf.st_gid = %d != gid = %d\n",
                  stbuf.st_gid,gid);
               goto skip;
               }
*/
            else if (!S_ISREG(stbuf.st_mode)) {
               goto skip;
               }
                                        /* Extract process ID.        */
            else {
               j = j + 1;
               for (i = 0; i + j < 14 && filename[i+j] != '\0'; i++) {
                  if (isdigit(filename[i+j]) == 0) {
                     goto skip;
                     }
                  else {
                     cpid[i] = filename[i+j];
                     }
                  }
               cpid[i] = '\0';
               sscanf(cpid,"%d",&procid);
                                        /* Process active             */
                                        /* Return process id          */
               if ((procid > 1) && 
                  ((kill (procid, 0) == 0) || (errno == EPERM))) {
                  *ierr = 0;
                  *pid = procid;
                  break;
                  }
                                        /* Remove dead process lockfil*/
               else {
                  unlink(pathname);
                  }
               }
                                        /* Skip this entry.           */
   skip:
            continue;
            }
         }
                                        /* Close directory.           */
      closedir (dfd);
      }

   return;
}
