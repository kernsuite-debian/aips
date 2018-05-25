#define Z_m70o2__
#include <stdio.h>
#include <fcntl.h>
#include <errno.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zm70o2_(int *fcb, char pname[MAXPNAME], int *ierr)
#else
   void zm70o2_(fcb, pname, ierr)
   int *fcb, *ierr ;
   char pname[MAXPNAME] ;
#endif
/*--------------------------------------------------------------------*/
/*! open an IIS Model 70/75 device                                    */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2000                                   */
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
/*  Open an IIS model 70 TV device for non-map,(single buffered) I/O. */
/*                                                                    */
/*  Inputs:                                                           */
/*     pname   C*48   Physical file name (e.g., "TVDEV1: ")           */
/*  Output:                                                           */
/*     fcb     I(*)   File control block for opened TV device         */
/*     ierr    I      Error return code: 0 => no error                */
/*                       2 => no such logical device                  */
/*                       3 => invalid device name                     */
/*                       6 => other open error                        */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   int llen, xlen, xlnb, jerr;
   register int i;
   char lognam[MAXPNAME], pathname[MAXAPATH] ;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Extract logical device     */
                                        /* name.                      */
   for (i = 0; i < MAXPNAME && pname[i] != ':'; i++)
      lognam[i] = pname[i];
   lognam[i] = '\0';
                                        /* Translate logical device   */
                                        /* name and null terminate.   */
   llen = MAXPNAME;
   xlen = MAXAPATH;
   ztrlo2_(&llen, lognam, &xlen, pathname, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 2;
      }
   else {
      pathname[xlnb] = '\0';
                                        /* Open the TV device         */
                                        /* read/write.                */
      if ((*(fcb + Z_fcbfd) = open (pathname, 2)) == -1) {
         *(fcb + Z_fcberr) = errno;
                                        /* Device doesn't exist.      */
         if (errno == ENOENT)
            *ierr = 3;
                                        /* Some other open error.     */
         else
            *ierr = 6;
         }
      }

   return;
}
