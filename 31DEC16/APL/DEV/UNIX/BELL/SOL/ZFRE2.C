#define Z_fre2__
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/statvfs.h>

#if __STDC__
   void zfre2_(int *nvol, char *avnam, int *vlen, int *totblk,
      float *pctful, int *frblks)
#else
   void zfre2_(nvol, avnam, vlen, totblk, pctful, frblks)
   int *nvol, *vlen, totblk[], frblks[] ;
   char *avnam;
   float pctful[];
#endif
/*--------------------------------------------------------------------*/
/*! return AIPS data disk free space information                      */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1999, 2009                                    */
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
/*  Get AIPS disk free space information.                             */
/*  Inputs:                                                           */
/*     nvol     I       Number of disks                               */
/*     vlen     I(*)    Length in characters per volume name          */
/*                      (listed before as output but routine never    */
/*                       did anything except read it)                 */
/*  Output:                                                           */
/*     avnam    C*(*)   Volume names                                  */
/*     totblk   I(*)    Total number of Mbytes on device              */
/*     pctful   R*4(*)  Percentage of device full                     */
/*     frblks   I(*)    Number of free Mbytes remaining               */
/*                                                                    */
/*  Solaris version:  No longer dependent on output from "df".  Use   */
/*  ----------------  "stavtfs()" instead.  This differs from the     */
/*  generic version which uses statfs, a function renamed by Solaris. */
/*  Neither of these are Posix.                                       */
/*  Note:  On SunOS 4.1 (fortran 1.3.1), do *NOT* use strncpy() on a  */
/*  -----  char pointer that is in effect a fortran character string. */
/*--------------------------------------------------------------------*/
{
                                        /* # bytes per AIPS-byte      */
   extern int Z_nbpab;
                                        /* loop variables             */
   register int i, j, k, l;
   off_t tot_user_blocks;
                                        /* length of DA0n names       */
   int vtemplen[35], vmaxlen, istat;
                                        /* strings for volnames       */
   char cmd[10], vlist[35][80], *vtemptr;
   static char hexc[37] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
                                        /* filesys. status structure  */
   statvfs_t *buf;
   float  xm, ym;
/*--------------------------------------------------------------------*/

  if ((buf = (statvfs_t *) calloc (1, sizeof(statvfs_t)))
     == 0) {
    perror ("ZFRE2 : can't allocate memory");
    return;
    }

   vmaxlen = 0;
                                        /* For each disk */
   for (i = 0; i < *nvol && i < 35; i++) {
                                        /* translate the env.var */
      sprintf (cmd, "DA0%c", hexc[i+1]);
      vtemptr = getenv(cmd);
                                        /* Fails?  Zero out stuff */
      if (vtemptr == 0) {
         sprintf(vlist[i], "undefined %s", cmd);
         totblk[i-1] = 0;
         pctful[i] = 0.0;
         frblks[i] = 0;
         }
      else {
                                        /* Success, fill in name */
                                        /* figure out length later */
         j = 0;
         if (strncmp(vlist[i], "/DATA/", 6) == 0) j = 6;
         strcpy(vlist[i], &vtemptr[j]);
                                        /* get filesystem status */
         if ((istat = statvfs(vtemptr, buf)) == 0) {
                                        /* Unix blocks to bytes, thence */
                                        /* to aips blocks.              */
            xm =  buf->f_frsize / (512. * Z_nbpab) / 1024.0;
            ym =  buf->f_bsize / (512. * Z_nbpab) / 1024.0;
            if (xm <= 0.0) xm = ym;
            totblk[i] = xm * buf->f_blocks + 0.5;
            frblks[i] = xm * buf->f_bavail + 0.5;
            tot_user_blocks = buf->f_blocks - (buf->f_bfree - buf->f_bavail);
            if (tot_user_blocks > 0)
               pctful[i] = 100.5 - (buf->f_bavail * 100.) / tot_user_blocks;
            else
               pctful[i] = 0.;
            }
         else {
	    sprintf(vlist[i], "can't read %s", vtemptr);
            totblk[i] = 0;
            pctful[i] = 0.0;
            frblks[i] = 0;
            }
         }
      vtemplen[i] = strlen(vlist[i]);
      vmaxlen = (vtemplen[i] > vmaxlen) ? vtemplen[i] : vmaxlen;
      }

   vmaxlen = *vlen;
   for (i = 0; i <= *nvol && i < 35; i++) {
                                        /* Copy it into output string */
                                        /* Note that strncpy may not  */
                                        /* work on fortran character  */
                                        /* strings...                 */
      k = (i)*vmaxlen;
      l = strlen(vlist[i]);
      for (j = 0; j < l && j < vmaxlen; j++) {
	 avnam[k+j] = vlist[i][j];
         }
                                        /* replace nulls with spaces */
      if (l < vmaxlen) {
         for (j = l; j < vmaxlen; j++) {
            if (avnam[k+j] == 0) {
               avnam[k] = ' ';
               }
            }
         }
      }
                                        /* Exit.                      */
   return;
}
