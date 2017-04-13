#include <errno.h>
#include <stdio.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>

ztact2_(len, name, pid, ierr)
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
char name[6];
int *len, *pid, *ierr;
/*--------------------------------------------------------------------*/
{
   extern int errno;
   char pname[7], cpid[7], filename[DIRSIZ], pathname[20];
   int procid, gid, fd;
   struct direct dirbuf;
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
      if (kill(*pid,0) == 0) {
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
      if ((fd = open("/tmp", 0)) != -1) {
                                        /* Search for filename of the */
                                        /* form 'pname.pid'.          */
         while (read(fd, (char*)&dirbuf, sizeof(dirbuf))>0) {
            if (dirbuf.d_ino == 0)
                                        /* Directory slot not in use. */
               continue;
                                        /* Compare.                   */
            for (i=0; i < 6 && pname[i] != '\0'; i++) {
               if (dirbuf.d_name[i] != pname[i]) {
                                        /* Mismatch, but perhaps its  */
                                        /* another dead AIPS process  */
                                        /* lock file.  If so, clean   */
                                        /* up (not implemented).      */

                  goto skip;
                  }
               }
                                        /* Get full filename.         */
            for (i = 0; i < 14 && dirbuf.d_name[i] != '\0'; i++) {
               filename[i] = dirbuf.d_name[i];
               if (filename[i] == '.') j = i;
               }
            filename[i] = '\0';
            sprintf(pathname,"/tmp/%s",filename);
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
            else if ((stbuf.st_mode & S_IFMT) != S_IFREG) {
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
               if (procid > 1 && kill(procid,0) == 0) {
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
      close(fd);
      }

   return;
}
