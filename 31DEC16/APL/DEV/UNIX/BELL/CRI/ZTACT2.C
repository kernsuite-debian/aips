#include <stdio.h>
#include <ctype.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/dir.h>
#include <sys/stat.h>
#define S_IFMT   0170000   /* type of file */
#define S_IFREG  0100000   /* regular */

ZTACT2(len, name, pid, ierr)
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
/* Tests to see if a process exists.                                  */
/*                                                                    */
/* INPUT:   name       Process name for which to test (6 char)        */
/* IN/OUT:  pid        Process id (if "0", determine from "name"      */
/*                     and return for subsequent use)                 */
/* OUTPUT:  ierr       Error return code:                             */
/*                        0 => found                                  */
/*                        1 => not found                              */
/*                                                                    */
/* UNICOS version.                                                    */
/*--------------------------------------------------------------------*/
char name[6];
int *len, *pid, *ierr;
/*--------------------------------------------------------------------*/
{
   extern int errno;
   char pname[7], cpid[7], filename[DIRSIZ], pathname[20], cmd[64],
      line[40];
   int procid, gid, fd;
   struct direct dirbuf;
   struct stat stbuf;
   register int i, j;
   FILE *pp, *popen();
                                        /* Assume task is not active. */
   *ierr = 1;
                                        /* Form null terminated       */
                                        /* process name.              */
   for (i = 0; i < 6 && name[i] != ' '; i++)
      pname[i] = name[i];
   pname[i] = '\0';

   if (*pid != 0) {
                                        /* Determine via process id   */
/*
      printf("ZTACT2: test via pid = %d\n",*pid);
*/
      if (kill(*pid,0) == 0 || errno == EPERM) {
         *ierr = 0;
         sprintf (cmd, "ps | grep '%d.*defunct'", *pid);
         pp = popen (cmd, "r");
         fgets (line, 40, pp);
         pclose (pp);
         if (line[0] != '\0') *ierr = 1;
         printf("ZTACT2: cmd     =  :%s:\n", cmd);
         printf("ZTACT2: line    =  :%s:\n", line);
         ISHELL ("ps");
         ISHELL (cmd);
      }
      else {
                                        /* Dead process lockfile.     */
                                        /* Remove.                    */
/*
         sprintf(pathname,"/tmp/%s.%d",pname,*pid);
         printf("ZTACT2: pathname = :%s:\n",pathname);
*/
         unlink(pathname);
      }
   }
   else {
                                        /* Determine pid from process */
                                        /* name as provided. Look for */
                                        /* regular files in /tmp with */
                                        /* names of the form          */
                                        /* 'pname.pid' (e.g.,         */
                                        /* MX1.12345)                 */
/*
      printf("ZTACT2: test via pname = %s\n",pname);
*/
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
/*
            printf("ZTACT2: d_name = %s\n",dirbuf.d_name);
*/
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
/*
            printf("ZTACT2: filename = :%s:\n",filename);
            printf("ZTACT2: pathname = :%s:\n",pathname);
*/
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
            else {
                                        /* Extract process ID.        */
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
/*
               printf("ZTACT2: cpid = :%s:  procid = %d\n",cpid,procid);
*/
                                        /* Process active?            */
               if (procid > 1 && kill(procid,0) == 0) {
                  *ierr = 0;
                                        /* Return process id for      */
                                        /* subsequent use.            */
                  *pid = procid;
 
                  sprintf (cmd, "ps | grep '%d.*defunct'", *pid);
                  pp = popen (cmd, "r");
                  fgets (line, 40, pp);
                  pclose (pp);
                  if (line[0] != '\0') *ierr = 1;
                  printf("ZTACT2: cmd     =  :%s:\n", cmd);
                  printf("ZTACT2: line    =  :%s:\n", line);
                  ISHELL ("ps");
                  ISHELL (cmd);
                                        /* Enough.                    */
                  break;
               }
               else {
                                        /* Dead process lockfile.     */
                                        /* Remove.                    */
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
