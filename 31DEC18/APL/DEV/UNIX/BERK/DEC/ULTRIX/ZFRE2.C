#define Z_fre2__
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/mount.h>

#if __STDC__
   void zfre2_(int *nvol, char *avnam, int *vlen, int *totblk,
      float *pctful, int *frblks)
#else
   zfre2_(nvol, avnam, vlen, totblk, pctful, frblks)
   int *nvol, *vlen, totblk[], frblks[] ;
   char *avnam;
   float pctful[];
#endif
/*--------------------------------------------------------------------*/
/*! return AIPS data disk free space information                      */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 2009                                         */
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
/*  DEC3100 version:  No longer dependent on output from "df".  Use   */
/*  ----------------  "statfs()" instead.  This is the Ultrix version */
/*  for DECstations (MIPS C compiler) as the statfs() call here       */
/*  returns a completely different structure than on other systems    */
/*  (even OSF/1!) and the include files are different too (Ultrix 4.3 */
/*  and DEC compiler).  This version supports >2G partitions.         */
/*--------------------------------------------------------------------*/
{
                                        /* # bytes per AIPS-byte      */
   extern int Z_nbpab;
                                        /* loop variables             */
   register int i, j, k, l;
                                        /* length of DA0n names       */
   int vtemplen[35], vmaxlen, istat;
                                        /* strings for volnames       */
   char cmd[10], vlist[35][80], *vtemptr;
   static char hexc[37] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
                                        /* filesys. status structure  */
   struct fs_data *buf;
   float xm;
/*--------------------------------------------------------------------*/
                                        /* struct is statfs on other */
                                        /* systems, fs_data here.    */
   if ((buf = (struct fs_data *) calloc (1, sizeof(struct fs_data)))
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
         totblk[i] = 0;
         pctful[i] = 0.0;
         frblks[i] = 0;
      } else {
                                        /* Success, fill in name */
                                        /* figure out length later */
         j = 0;
         if (strncmp(vlist[i], "/DATA/", 6) == 0) j = 6;
         strcpy(&vlist[i], &vtemptr[j]);
                                        /* get filesystem status */
                                        /* return value 1 means ok here, */
                                        /* most other systems use 0. */
         if ((istat = statfs(vtemptr, buf)) == 1) {

                                        /* Unix blocks to bytes,      */
                                        /* thence to aips blocks.     */
                                        /* Note here buf->fd_req.xxx, */
                                        /* on other UNIXen buf->f_xxx */
                                        /* in most cases.  Ultrix has */
                                        /* btot in 1k blocks (see man */
                                        /* page for getmnt(2))        */
                                        /* Avoid overflow via "xm"    */
	    xm = 1024. / (512. * Z_nbpab) / 1024.;
            totblk[i] = xm * buf->fd_req.btot + 0.5;
            frblks[i] = xm * buf->fd_req.bfreen + 0.5;
            pctful[i] = 100.5 - (buf->fd_req.bfreen * 100. /
                                (buf->fd_req.btot -
			     (buf->fd_req.bfree - buf->fd_req.bfreen)));
         } else {
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
   for (i = 0; i < *nvol && i < 35; i++) {
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
