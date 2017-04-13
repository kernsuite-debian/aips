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
/*     avnam    C*(*)   Volume names                                  */
/*     vlen     I(*)    Length in characters per volume name          */
/*     totblk   I(*)    Total number of Mbytes on device              */
/*     pctful   R*4(*)  Percentage of device full                     */
/*     frblks   I(*)    Number of free Mbytes remaining               */
/*                                                                    */
/*  UNICOS version.  Uses output of "df" which is HIGHLY unreliable.  */
/*  It is far preferable to use statfs, and in fact the generic       */
/*  APLUNIX version of ZFRE2.C does indeed use this call.  However,   */
/*  that routine was modified at a time when the AIPS group had no    */
/*  easy access to a Cray or Unicos system, hence this module has not */
/*  been updated accordingly.                                         */
/*--------------------------------------------------------------------*/
{
                                        /* # bytes per AIPS-byte      */
   extern int Z_nbpab;
   int itbytes, iubytes, ifbytes, ipct, icolon;
   register int i, j, k;
   char cmd[256], line[80], vnam[32];
   static char hexc[37] = "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ";
   FILE *pp, *popen();
/*--------------------------------------------------------------------*/
                                        /* Build "df" command line.   */
   sprintf (cmd, "df ");
   for (i = 1; i <= *nvol && i <= 35; i++)
      sprintf (cmd, "%s $DA0%X", cmd, hexc[i]);
                                        /* Open a pipe, issue the     */
                                        /* command and parse the      */
                                        /* output.                    */
   pp = popen (cmd, "r");
   for (i = -1; i < *nvol && fgets (line, 80, pp) != NULL; i++) {
                                        /* Skip the header from "df". */
      if (i == -1) continue;
      itbytes = iubytes = ifbytes = ipct = 0;
      sscanf (line, "%s%d%d%d%d", vnam, &itbytes, &iubytes, &ifbytes,
         &ipct);
                                        /* Get continuation line      */
      icolon = 0;
      for (i = 0; i < 32 && icolon == 0; i++)
         if (vnam[i] = ':') icolon = i;
      if ((icolon != 0) && (itbytes == 0)) {
         fgets (line, 80, pp) ;
         sscanf (line, "%d%d%d%d", &itbytes, &iubytes, &ifbytes, &ipct);
         }
      j = *vlen * i;
      for (k = j; k < *vlen + j && vnam[k-j] != ' ' &&
         vnam[k-j] != '\0';  k++) {
	 avnam[k] = vnam[k-j];
         }
      for (k = k; k < *vlen + j; k++)
         avnam[k] = ' ';
      totblk[i] = 2. * (itbytes / Z_nbpab) / 1024. + 0.5;
      frblks[i] = 2. * (ifbytes / Z_nbpab) / 1024. + 0.5;
      pctful[i] = 1.0 * ipct;
      }
                                        /* Close the pipe.            */
   pclose (pp);
                                        /* Exit.                      */
   return;
}
