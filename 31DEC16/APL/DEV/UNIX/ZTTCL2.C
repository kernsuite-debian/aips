#define Z_ttcl2__
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zttcl2_(int *fcb, int *ierr)
#else
   void zttcl2_(fcb, ierr)
   int *fcb, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! close a Message window                                            */
/*# Graphics                                                          */
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
/*  Close a Message window.                                           */
/*  Inputs:                                                           */
/*     fcb    I(*)   File control block for opened MSGSRV device      */
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => ok                       */
/*                      1 => error                                    */
/*  Generic UNIX version.                                             */
/*  Contains code that will reset TTDEV to TTDEV00 if there is an     */
/*  error in the close on the socket.                                 */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4, Z_nfcber, Z_nfcbfd;
                                        /* flag from ZTTOP2 for speed */
   extern int remembered;
   int fd, llen, tlen, jerr;
   char lnam[6], tlnam[8];
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *(fcb + Z_nfcber) = 0;
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZTTOP2.    */
   fd = *(fcb + Z_nfcbfd);
                                        /* Close Socket to MSGSERVER. */
   if (close (fd) == -1) {
      *(fcb + Z_nfcber) = errno;
      *ierr = 1;
                                        /* is this even necessary?    */
      remembered = 0;
      strcpy (lnam, "TTDEV");
      strcpy (tlnam, "TTDEV00");
      llen = strlen(lnam);
      tlen = strlen(tlnam);
      zcrlog_ (&llen, lnam, &tlen, tlnam, &jerr);
      }
   *(fcb + Z_nfcbfd) = 0;

   return;
}
