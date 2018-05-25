#define Z_fre2__
#include <stdio.h>

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
/*  Output:                                                           */
/*     avnam   C*(*)   Volume names                                  */
/*     vlen     I(*)    Length in characters per volume name          */
/*     totblk   I(*)    Total number of 256-integer blocks on  device */
/*     pctful   R*4(*)  Percentage of device full                     */
/*     frblks   I(*)    Number of free 512 AIPS-byte blocks remaining */
/*                                                                    */
/*  Masscomp version - depends on output and options to local "df"    */
/*  command; this is HIGHLY UNRELIABLE and not recommended.  As NRAO  */
/*  no longer has access to a masscomp system, this routine cannot be */
/*  upgraded by us as the APLUNIX version has been, to use statfs().  */
/*  NOTE - Eric has added a guess as to where the volume name actually*/
/*  appears.  This need to be checked and corrected.*******************/
/*--------------------------------------------------------------------*/
{
                                        /* # bytes per AIPS-byte      */
   extern int Z_nbpab;
   int itbytes, iubytes, ifbytes, ipct;
   register int i, j, k;
   float fblk, tblk, pct;
   char cmd[512], line[80], vnam[32];
   static char hexc[37] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   FILE *pp, *popen();
/*--------------------------------------------------------------------*/
                                        /* Build "df" command line.   */
   sprintf (cmd, "df -t ");
   for (i = 1; i <= *nvol && i <= 35; i++)
      sprintf (cmd, "%s $DA0%c", cmd, hexc[i]);
                                        /* Open a pipe, issue the     */
                                        /* command and parse the      */
                                        /* output.                    */
   pp = popen (cmd, "r");
   for (i = -1; i < *nvol && fgets (line, 80, pp) != NULL; i++) {
                                        /* Skip the header from "df". */
      if (i == -1) continue;
      sscanf (line, "%s%*s%*s%f", vnam, &fblk);
      fgets (line, 80, pp);
      sscanf(line, "%*s%f", &tblk);
      pct = 100.0 * (tblk - fblk) / tblk;
      totblk[i] = 2. * (tblk / (1. * Z_nbpab)) / 1024. + 0.5;
      pctful[i] = pct;
      frblks[i] = 2. * (fblk / (1. * Z_nbpab)) / 1024. + 0.5;
      j = *vlen * i;
      for (k = j; k < *vlen + j && vnam[k-j] != ' ' &&
         vnam[k-j] != '\0';  k++) {
	 avnam[k] = vnam[k-j];
         }
      for (k = k; k < *vlen + j; k++)
         avnam[k] = ' ';
      }
                                        /* Close the pipe.            */
   pclose(pp);

   return;
}
