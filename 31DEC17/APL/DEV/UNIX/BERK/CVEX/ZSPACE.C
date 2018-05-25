#include <ctype.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/timeb.h>
#include <sys/stat.h>
#include <sys/time.h>

/* As a hint to see if we use new style or old-style directory
 *  reading, we see if _POSIX_SORCE is defined. If it is, we almost
 *  certainly want new, otherwise we probably want old. If this
 *  doesn't work manually pick the right one.
 */

#ifdef _POSIX_SOURCE
#include <dirent.h>
#else
#include <sys/dir.h>
#define dirent direct
#endif /* _POSIX_SOURCE */


zspace_(dirnam, maxusr, kblks, kbdays, ierr, ld)
/*--------------------------------------------------------------------*/
/*! return disk usage summaries since last real disk use              */
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
/*  Returns arrays containing kiloblock disk usages and average       */
/*  kiloblock-days since last "true" access for user numbers 1 to     */
/*  "maxusr".                                                         */
/*  Inputs:                                                           */
/*     dirnam   C*(*)  Directory pathname from which to gather stats  */
/*     maxusr   I      Maximum user number                            */
/*  Outputs:                                                          */
/*     kblks    R(*)   Array of user based space consumption in       */
/*                     kiloblocks (512 AIPS bytes/block)              */
/*     kbdays   R(*)   Array of user based average "true" access      */
/*                     time in kiloblock-days                         */
/*     ierr     I      Error return code: 0 => okay                   */
/*                        1 => error                                  */
/*  Convex version - not done anywhere else currently                 */
/*--------------------------------------------------------------------*/
char dirnam[];
int *maxusr, *ierr, ld;
float kblks[], kbdays[];
/*--------------------------------------------------------------------*/
{
   DIR *dirp;
   struct dirent *dp;
   char dir[64];
   struct stat stbuf;
   struct timeb tbuf;

   char fname[256], pathname[256], hexusr[4];
   register int i, j, k, l;
   int userno;
   long int truesize;
   float kblocks;
   int strcmp();
/*--------------------------------------------------------------------*/
                                        /* Get current time.         */
   ftime(&tbuf);
                                        /* Form null terminated      */
                                        /* directory pathname.       */
   for (i = 0; i < ld && dirnam[i] != ' '; i++)
      dir[i] = dirnam[i];
   dir[i] = '\0';
/*
   printf("ZSPACE: open directory %s\n",dir);
*/
                                        /* Open directory.            */
   if ((dirp = opendir(dir)) == NULL) {
/*    perror("ZSPACE"); */
      *ierr = 1;
/*    goto exit;  */
      }
                                        /* Collect statistics on      */
                                        /* AIPS physical filenames.   */
   else {
      for (dp = readdir(dirp); dp != NULL; dp = readdir(dirp)) {
                                        /* Skip self and parent.      */
         if (strcmp(dp->d_name,".")  == 0 ||
             strcmp(dp->d_name,"..") == 0)
            goto skip;
         userno = 0;
                                        /* AIPS physical filename.    */
         if ((l = sindex(dp->d_name,";1")) != -1) {
            if ((k = sindex(dp->d_name,".")) == -1)
               goto skip;
            for (i = 0, j = k + 1; j < l; i++, j++)
               hexusr[i] = dp->d_name[j];
            hexusr[i] = '\0';
            sscanf(hexusr,"%x",&userno);
            }
                                        /* Get filename.              */
         for (i = 0; i < 256 && dp->d_name[i] != '\0'; i++)
            fname[i] = dp->d_name[i];
         fname[i] = '\0';
         sprintf(pathname,"%s/%s",dir,fname);
         if (stat(pathname, &stbuf) == -1)
            perror("ZSPACE");
         else {
/*
            truesize = stbuf.st_blocks * 512;
            if (userno == 0) {
            printf("\n");
            printf("ZSPACE: filename           = %s\n",fname);
            printf("ZSPACE: AIPS user #        = %s = %d\n",hexusr,userno);
            printf("ZSPACE: inode              = %d\n",stbuf.st_ino);
            printf("ZSPACE: permissions        = %o\n",stbuf.st_mode & 0777);
            printf("ZSPACE: links              = %d\n",stbuf.st_nlink);
            printf("ZSPACE: owner id           = %d\n",stbuf.st_uid);
            printf("ZSPACE: group id           = %d\n",stbuf.st_gid);
            printf("ZSPACE: size (bytes)       = %d\n",stbuf.st_size);
            printf("ZSPACE: block size         = %d\n",stbuf.st_blksize);
            printf("ZSPACE: size (blocks)      = %d\n",stbuf.st_blocks);
            printf("ZSPACE: size (true)        = %d\n",truesize);
            printf("ZSPACE: last access        = %s",
               asctime(localtime(&stbuf.st_atime)));
            printf("ZSPACE: last modification  = %s",
               asctime(localtime(&stbuf.st_mtime)));
            printf("ZSPACE: last status change = %s",
               asctime(localtime(&stbuf.st_ctime)));
            }
*/
                                        /* Ascribe all other usage to */
                                        /* user # 1, the AIPS manager.*/
            if (userno > 0) userno = userno - 1;
                                        /* Use bytes so that we get   */
                                        /* kiloblocks correct.        */
            kblocks = (float)(stbuf.st_size) / (1024.0 * 1024.0);
            kblks[userno] = kblks[userno] + kblocks;
            kbdays[userno] = kbdays[userno] +
               kblocks * (tbuf.time - stbuf.st_atime) / 86400.0;
            }
skip:    continue;
         }
                                        /* Close directory.           */
      closedir(dirp);
      }
   for (i = 0; i < *maxusr; i++) if (kblks[i] > 0.0) {
      kbdays[i] = kbdays[i] / kblks[i];
/*
      printf("ZSPACE: user %x = %d   %f Kblks  %f Kblk-days\n",
         i+1,i+1,kblks[i],kbdays[i]);
*/
      }
exit:
   return;
}
/*--------------------------------------------------------------------*/
sindex(s1, s2)
char s1[], s2[];
/*--------------------------------------------------------------------*/
{
   register int i, j, k;
/*--------------------------------------------------------------------*/
   for (i = 0; s1[i] != '\0'; i++) {
      for (j = i, k = 0; s2[k] != '\0' && s1[j] == s2[k]; j++, k++);
                                        /* Found.                     */
      if (s2[k] == '\0')
         return(i);
      }
                                        /* Not found.                 */
   return(-1);

}
