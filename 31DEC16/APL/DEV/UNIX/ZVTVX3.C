#define Z_vtvx3__
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

#if __STDC__
   void zvtvx3_ (int *fcb, int *bufsw, char *buffer, int *ierr)
#else
   void zvtvx3_ (fcb, bufsw, buffer, ierr)
   int *fcb, *bufsw, *ierr;
   char buffer[];
#endif
/*--------------------------------------------------------------------*/
/*! reads/writes from/to client (virtual TV) for the server (real TV) */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997                                         */
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
/*  ZVTVX3 reads into TVMON (on the computer with the real TV) from   */
/*  the computer running AIPS VTV code and returns answers and error  */
/*  codes.                                                            */
/*  Inputs:                                                           */
/*     fcb      I      File descriptor                                */
/*     bufsw    I      Number words to send (incl header); if = 0,    */
/*                     implies this is a read instead.                */
/*  In/out:                                                           */
/*     buffer   I(*)   8-word header plus data buffer: in             */
/*                     FITS-standard 16-bit integer form!!!!!         */
/*  Output:                                                           */
/*     ierr     I      Error: 0 => okay                               */
/*                        3 => IO error                               */
/*                        4 => End of File                            */
/*  BSD 4.2 version.                                                  */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
   extern int Z_bytflp;
   int cfd, nbytes, mbytes, lbytes, ioff, kwords, iwords, jwords;
   char *iaddr;
/*--------------------------------------------------------------------*/
   *ierr = errno = 0;
   cfd = *(fcb + Z_fcbsp1);
                                        /* write the header + data    */
   if (*bufsw > 0) {
      ioff = 0;
                                        /* Loop invariant: elements   */
                                        /* 0 to ioff-1 of buffer have */
                                        /* been written to the socket */
      for (nbytes = 2 * (*bufsw) ; nbytes != 0; nbytes -= mbytes) {
         lbytes = (nbytes > 1024) ? 1024 : nbytes ;
         iaddr = &(buffer[ioff]) ;
         if ((mbytes = write(cfd, iaddr, lbytes)) == 0) {
            perror("ZVTVX3 WRITE ERROR");
            *ierr = 3;
            goto exit;
            }
         ioff += mbytes;
         }
      }
                                        /* read new header            */
   else {
      ioff = 0;
                                        /* Loop invariant: elements   */
                                        /* 0 to ioff-1 of buffer have */
                                        /* been read from the socket  */
      for (nbytes = 16; nbytes != 0; nbytes -= mbytes) {
         iaddr = &(buffer[ioff]);
         if ((mbytes = read(cfd, iaddr, nbytes)) <= 0) {
            if (mbytes == 0)
               *ierr = 4;
            else {
               *ierr = 3;
               perror("ZVTVX3 READ ERROR");
               }
            goto exit;
            }
         ioff += mbytes;
         }
                                        /* read new data              */
      ioff = 16;
      if ((Z_bytflp == 1) || (Z_bytflp == 3)) {
         iwords = buffer[11] ;
         jwords=  buffer[10] ;
         }
      else {
         iwords = buffer[10] ;
         jwords = buffer[11] ;
         }
      if (iwords < 0) iwords = iwords + 256 ;
      if (jwords < 0) jwords = jwords + 256 ;
      kwords = 256 * iwords + jwords ;
                                        /* Loop invariant: elements  */
                                        /* 16 to ioff-1 of buffer    */
                                        /* have been read from the   */
                                        /* socket                    */
      for (nbytes = 2 * kwords; nbytes > 0; nbytes -= mbytes) {
         lbytes = (nbytes > 1024) ? 1024 : nbytes ;
         iaddr = &(buffer[ioff]) ;
         if ((mbytes = read(cfd, iaddr, lbytes)) <= 0) {
            if (mbytes == 0)
               *ierr = 4;
            else
               *ierr = 3;
            perror("ZVTVX3 READ ERROR");
            goto exit;
            }
         ioff += mbytes;
         }
      }


exit:
   return;
}

