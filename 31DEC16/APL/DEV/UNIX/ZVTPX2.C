#define Z_vtpx2__
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>

#if __STDC__
   void zvtpx2_ (int *fcb, int *bufsw, int *bufsr, char *buffer,
      int *ierr)
#else
   void zvtpx2_ (fcb, bufsw, bufsr, buffer, ierr)
   int *fcb, *bufsw, *bufsr, *ierr;
   char buffer[];
#endif
/*--------------------------------------------------------------------*/
/*! writes/reads to/from server for the client (virtual tape) machine */
/*# Tape Z2                                                           */
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
/*  ZVTPX2 transfers data between the clent (virtual tape) machine    */
/*  and the server (actual tape) computer                             */
/*  Inputs:                                                           */
/*     fcb     I(*)   File descriptor                                 */
/*     bufsw   I      Number of data words (each 32 bits) to send     */
/*                    beyond the header.                              */
/*  In/out:                                                           */
/*     bufsr   I      Number of data words (each 32 bits) to read back*/
/*                    beyond the header: really set by TPMON          */
/*     buffer  I(*)   Data buffer: header + data                      */
/*  Output:                                                           */
/*     ierr    I      Error: 0 => okay                                */
/*  UNIX Berkeley version - read/write from socket.                   */
/*  The header format is, in 32-bit words:                            */
/*      1,2    H(2)   Subroutine name                                 */
/*      3      I      bufsw                                           */
/*      4      I      bufsr                                           */
/*      5      I      ierr                                            */
/*  Data begins in the 6th 32-bit word in any format agreed between   */
/*  TPMON and the relevant subroutine.  E.g.,                         */
/*     ZTAPE:  header   = 'ZTAPE    ', 3, 0, 0                        */
/*             data in  = OPcode, LUN, Count                          */
/*             out      = only header word 5 (IERR) used              */
/*     ZTPMIO: header   = 'ZTPMIO  ',  4, 2+n, 0              (read)  */
/*             data in  = 'READ', LUN, Nbytes, Buf#                   */
/*             data out = SysError, BytesXfer, buffer(N)              */
/*     ZTPMIO: header   = 'ZTPMIO  ',  4+n, 2, 0              (write) */
/*             data in  = 'WRIT', LUN, Nbytes, Buf#, buffer(N)        */
/*             data out = SysError, BytesXfer                         */
/*             where N = # local words to Xfer Nbytes of data         */
/*                   n = # 32-bit words to Xfer Nbytes of data        */
/*             header word 5 (IERR) also used on output.              */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcberr, Z_sndbuf, Z_rcvbuf, Z_sndini, Z_rcvini;
   int nbytes, mbytes, lbytes, sfd, ioff, lwords, iwords, jwords,
      kwords, optlen;
   char *iaddr;
/*--------------------------------------------------------------------*/
   errno = 0;
   sfd = *(fcb + Z_fcbfd) ;
   *ierr = 0;
                                        /* write the header + data    */
   ioff = 0;
                                        /* Loop invariant: elements   */
                                        /* 0 to ioff-1 of buffer have */
                                        /* been written to the socket */
                                        /* Transfer data:             */

   nbytes = 4 * (*bufsw) + 20 ;

   if ((Z_sndbuf == Z_sndini) && (nbytes > Z_sndini)) {
      optlen = sizeof (Z_sndbuf);
      Z_sndbuf = 29000;
      if (setsockopt (sfd, SOL_SOCKET, SO_SNDBUF, (char *) &Z_sndbuf,
         optlen) < 0) {
         perror ("ZVTPX2 increase sndbuf size");
         Z_sndbuf = Z_sndini+1;
         }
      else
         if (Z_sndini > 0) fprintf (stderr,
            "ZVTPX2 increased send buffer size\n");
      }

/* fprintf (stderr,
      "ZVTPX2 intends %d data bytes to TPMON\n", nbytes); */

   for (nbytes = 4 * (*bufsw) + 20 ; nbytes > 0 ; nbytes -= mbytes) {
      lbytes = nbytes ;
      iaddr = &(buffer[ioff]) ;
      if ((mbytes = write(sfd, iaddr, lbytes)) == -1) {
         perror("ZVTPX2 WRITE DATA ERROR");
         *ierr = 3;
         goto exit;
         }
      ioff += mbytes;
/*   fprintf (stderr,
        "ZVTPX2 sends %d data bytes to TPMON\n", mbytes); */
      }
                                        /* read the header + data     */
   ioff = 0;
                                        /* Loop invariant: elements   */
                                        /* 0 to ioff-1 have been read */
                                        /* from the socket            */

/* fprintf (stderr, "ZVTPX2 intends to read header from TPMON\n"); */

   for (nbytes = 20 ; nbytes > 0; nbytes -= mbytes) {
      iaddr = &(buffer[ioff]) ;
      if ((mbytes = read(sfd, iaddr, nbytes)) <= 0) {
         if (mbytes == 0)              /* End of file indicator      */
            *ierr = 4;
         else
            *ierr = 3;
         perror("ZVTPX2 READ HEADER ERROR");
         goto exit;
         }
      ioff += mbytes;
/*    fprintf (stderr, "ZVTPX2 reads %d bytes from TPMON\n",mbytes); */
      }
                                       /* number data words          */
                                       /* FITS byte order here       */
   iwords = buffer[13] ;
   jwords = buffer[14] ;
   kwords = buffer[15] ;
   if (iwords < 0) iwords = iwords + 256 ;
   if (jwords < 0) jwords = jwords + 256 ;
   if (kwords < 0) kwords = kwords + 256 ;
   lwords = 256 * (256 * iwords + jwords) + kwords ;
   *bufsr = lwords;

   nbytes = 4 * (lwords) ;

   if ((Z_rcvbuf == Z_rcvini) && (nbytes > Z_rcvini)) {
      optlen = sizeof (Z_rcvbuf);
      Z_rcvbuf = 29000;
      if (setsockopt (sfd, SOL_SOCKET, SO_RCVBUF, (char *) &Z_rcvbuf,
         optlen) < 0) {
         perror ("ZVTPX2 increase rcvbuf size");
         Z_rcvbuf = Z_rcvini+1;
         }
      else
         if (Z_rcvini > 0) fprintf (stderr,
            "ZVTPX2 increased receive buffer size\n");
      }

/* fprintf (stderr,
     "ZVTPX2 intends to read %d data bytes from TPMON\n", nbytes); */

   for (nbytes = 4 * lwords ; nbytes > 0; nbytes -= mbytes) {
      lbytes = nbytes ;
      iaddr = &(buffer[ioff]) ;
      if ((mbytes = read(sfd, iaddr, lbytes)) <= 0) {
         if (mbytes == 0)              /* End of file indicator      */
            *ierr = 4;
         else
            *ierr = 3;
         perror("ZVTPX2 READ DATA ERROR");
         goto exit;
         }
      ioff += mbytes;
/*    fprintf (stderr, "ZVTPX2 reads %d bytes from TPMON\n",mbytes); */
      }

exit:
   if (*ierr != 0) *(fcb + Z_fcberr) = errno;
   return ;
}
