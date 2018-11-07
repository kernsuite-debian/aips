#define Z_vtpx3__
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>

#if __STDC__
   void zvtpx3_ (int *fcb, int *bufsw, char *buffer, int *ierr)
#else
   void zvtpx3_ (fcb, bufsw, buffer, ierr)
   int *fcb, *bufsw, *ierr;
   char buffer[];
#endif
/*--------------------------------------------------------------------*/
/*! reads/writes from/to client (virtual TP) for the server (real TP) */
/*# TP-IO                                                             */
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
/*  ZVTPX3 reads into TPMON (on the computer with the real tape) from */
/*  the computer running AIPS virtual tape code and returns answers   */
/*  and error codes.                                                  */
/*  Inputs:                                                           */
/*     fcb      I      File descriptor                                */
/*     bufsw    I      Number words to send (incl header); if = 0,    */
/*                     implies this is a read instead.                */
/*  In/out:                                                           */
/*     buffer   I(*)   5-word header plus data buffer: in             */
/*                     FITS-standard 32-bit integer form!!!!!         */
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
   extern int Z_bytflp, Z_sndbuf, Z_rcvbuf, Z_sndini, Z_rcvini;
   int cfd, nbytes, mbytes, lbytes, ioff, lwords, iwords, jwords,
      kwords, optlen;
   char *iaddr;
/*--------------------------------------------------------------------*/
   *ierr = errno = 0;
   cfd = *(fcb + Z_fcbsp1);
                                        /* write the header + data    */
   if (*bufsw > 0) {
      ioff = 0;

      nbytes = 4 * (*bufsw) ;

      if ((Z_sndbuf == Z_sndini) && (nbytes > Z_sndini)) {
         optlen = sizeof (Z_sndbuf);
         Z_sndbuf = 29000;
         if (setsockopt (cfd, SOL_SOCKET, SO_SNDBUF,
            (char *) &Z_sndbuf, optlen) < 0) {
            perror ("ZVTPX3 increase sndbuf size");
            Z_sndbuf = Z_sndini+1;
         }
      else
         if (Z_sndini > 0) fprintf (stderr,
            "ZVTPX3 increased send buffer size\n");
         }

/*    fprintf (stderr,
         "ZVTPX3 to send back %d data bytes\n", nbytes); */

      for (nbytes = 4 * (*bufsw) ; nbytes > 0; nbytes -= mbytes) {
         lbytes = nbytes ;
         iaddr = &(buffer[ioff]) ;
         if ((mbytes = write(cfd, iaddr, lbytes)) == 0) {
            perror("ZVTPX3 WRITE DATA ERROR");
            *ierr = 3;
            goto exit;
            }
         ioff += mbytes;
/*      fprintf (stderr,
           "ZVTPX3 sends back %d data bytes\n", mbytes); */
         }
      }
                                        /* read new header            */
   else {
      ioff = 0;
                                        /* Loop invariant: elements   */
                                        /* 0 to ioff-1 of buffer have */
                                        /* been read from the socket  */

/*    fprintf (stderr, "ZVTPX3 to read header\n"); */

      for (nbytes = 20; nbytes > 0; nbytes -= mbytes) {
         iaddr = &(buffer[ioff]);
         if ((mbytes = read(cfd, iaddr, nbytes)) <= 0) {
            if (mbytes == 0)
               *ierr = 4;
            else {
               *ierr = 3;
               perror("ZVTPX3 READ HEADER ERROR");
               }
            goto exit;
            }
         ioff += mbytes;
/*      fprintf (stderr, "ZVTPX3 reads %d header bytes\n",mbytes); */
         }
                                        /* read new data              */
      ioff = 20;
                                        /* FITS byte order here       */
      iwords = buffer[9] ;
      jwords = buffer[10] ;
      kwords = buffer[11] ;
      if (iwords < 0) iwords = iwords + 256 ;
      if (jwords < 0) jwords = jwords + 256 ;
      if (kwords < 0) kwords = kwords + 256 ;
      lwords = 256 * (256 * iwords + jwords) + kwords ;
                                        /* Loop invariant: elements  */
                                        /* 16 to ioff-1 of buffer    */
                                        /* have been read from the   */
                                        /* socket                    */

      nbytes = 4 * lwords;

   if ((Z_rcvbuf == Z_rcvini) && (nbytes > Z_rcvini)) {
      optlen = sizeof (Z_rcvbuf);
      Z_rcvbuf = 29000;
      if (setsockopt (cfd, SOL_SOCKET, SO_RCVBUF, (char *) &Z_rcvbuf,
         optlen) < 0) {
         perror ("ZVTPX3 increase rcvbuf size");
         Z_rcvbuf = Z_rcvini+1;
         }
      else
         if (Z_rcvini > 0) fprintf (stderr,
            "ZVTPX3 increased receive buffer size\n");
      }

/*    fprintf (stderr,
         "ZVTPX3 intends to read %d data bytes\n", nbytes); */

      for (nbytes = 4 * lwords; nbytes > 0; nbytes -= mbytes) {
         lbytes = nbytes ;
         iaddr = &(buffer[ioff]) ;
         if ((mbytes = read(cfd, iaddr, lbytes)) <= 0) {
            if (mbytes == 0)
               *ierr = 4;
            else
               *ierr = 3;
            perror("ZVTPX3 READ DATA ERROR");
            goto exit;
            }
         ioff += mbytes;
/*       fprintf (stderr, "ZVTPX3 reads %d data bytes\n", mbytes); */
         }
      }


exit:
   return;
}

