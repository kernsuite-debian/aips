#define Z_zvtvgc__
#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
   struct sockaddr_un fromb, serverb;
   struct sockaddr_in fromb_in, serverb_in;
   struct servent *spb_in;

#if __STDC__
   void zvtvgc_(int *fcb, int *ierr)
#else
   void zvtvgc_(fcb, ierr)
   int *fcb, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! close & reopen connection in server (real-TV) to client (VTV)     */
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
/*  opens the connection in TVMON to the remote machine which is      */
/*  running the AIPS VTV (Virtual TV) code.  This differs from ZVTVO3 */
/*  in that the socket is already there and an old connection must be */
/*  closed before a new one can be accepted.  Called by ZVTVRX.       */
/*  Outputs:                                                          */
/*     fcb   I(*)   File descriptor                                   */
/*     ierr  I      Error: 0 => okay                                  */
/*   SUN (and other Berkeley UNIX ?) version                          */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
   char device[80], shvar[10];
   int i, j, len, cfd, sfd;
   char *machine_in;
   struct sockaddr_un client;
   struct sockaddr_in client_in;
   struct hostent *hp_in;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   errno = 0;
   sfd = *(fcb + Z_fcbfd);
   cfd = *(fcb + Z_fcbsp1);
                                        /* shutdown connection        */
   shutdown(cfd, 0);
   close(cfd);
                                        /* accept new connection      */
   len = sizeof(fromb_in);
   if ((cfd = accept(sfd, (struct sockaddr *) &fromb_in, &len)) < 0) {
      perror("ZVTVGC accept (INET) error");
      *ierr = 1;
      *(fcb + Z_fcberr) = errno;
      }
   else {
      *(fcb + Z_fcbsp1) = cfd;
      }

   return;
}
