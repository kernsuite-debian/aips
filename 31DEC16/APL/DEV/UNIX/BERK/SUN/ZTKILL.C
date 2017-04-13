#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_tkill__
#include <unistd.h>
#include <sys/types.h>
#include <stdio.h>
#include <ctype.h>
#include <dirent.h>
#include <sys/stat.h>
#include <signal.h>

#if __STDC__
   void ztkill_(char *dname, int inpid[4], int *ierr)
#else
   ztkill_(dname, inpid, ierr)
   char dname[];
   int inpid[4], *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! deletes (or kills) the specified process                          */
/*# Z System                                                          */
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
/*  Tests to see if task 'dname' is running and if so, issues soft    */
/*  kill (signal 15) for it.                                          */
/*  Input:                                                            */
/*     dname   C*6   AIPS task name to be killed                      */
/*     inpid   I[4]  Process "ID" code:                               */
/*                      inpid[0] user number  (0 => any user)         */
/*                      inpid[1-3] process #  (0 => unknown)          */
/*  Output:                                                           */
/*     ierr    I     Error return code: 0 => ok                       */
/*                      1 => error                                    */
/*  POSIX (Bell) UNIX version                                         */
/*--------------------------------------------------------------------*/
{
   char pname[7], cpid[7], filename[14], pathname[20];
   pid_t pid;
   gid_t gid;
   uid_t aipsid;
   DIR *dfd;
   struct dirent *dirp;
   struct stat stbuf;
   register int i, j;
/*--------------------------------------------------------------------*/
                                        /* Assume task is not active. */
   *ierr = 1;
                                        /* Form null terminated       */
                                        /* process name.              */
   for (i = 0; i < 6 && dname[i] != ' '; i++)
      pname[i] = dname[i];
   pname[i] = '\0';
                                        /* User number?  Not really  */
                                        /* implemented in AIPS yet.  */
   if (inpid[0] == 0)
      aipsid = 0;
   else
      aipsid = geteuid();
                                        /* Kill via process id as    */
                                        /* provided.                 */
   if (inpid[1] != 0) {
      pid = inpid[1];
      if ((kill(pid,0) == 0) && (kill(pid,SIGTERM) == 0))
         *ierr = 0;
      else perror("ZTKILL");
      }
                                        /* Determine pid from process */
                                        /* name as provided. Look for */
                                        /* regular files in /tmp with */
                                        /* names of the form          */
                                        /* 'pname.pid' (MX1.12345)    */
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
                                        /* Mismatch                   */
               if (dirp->d_name[i] != pname[i]) {
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
               sscanf (cpid, "%d", &pid);
                                        /* Kill process.              */
               if (pid > 1 && (kill (pid, 0) == 0) &&
                  (kill (pid, SIGTERM) == 0)) {
                  *ierr = 0;
                  unlink (pathname);
                  break;
                  }
                                        /* Remove dead process lockfil*/
               else {
                  unlink (pathname);
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
