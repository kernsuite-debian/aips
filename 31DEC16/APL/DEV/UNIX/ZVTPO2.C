#define Z_vtpo2__
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>
   int Z_sndbuf, Z_rcvbuf, Z_sndini, Z_rcvini;

#if __STDC__
   void zvtpo2_(int *fcb, int *plen, char *pname, int *ierr)
#else
   void zvtpo2_(fcb, plen, pname, ierr)
   int *fcb, *plen, *ierr;
   char pname[24];
#endif
/*--------------------------------------------------------------------*/
/*! open connection in client to server of remote, real tape          */
/*# Tape Z2                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997                                          */
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
/*  ZVTPO2 opens the connection in the Virtual tape program to the    */
/*  remote computer which provides the actual tape device.            */
/*  Inputs:                                                           */
/*     FCB     I(*)   File descriptor                                 */
/*     PLEN    I      Length of PNAME                                 */
/*     PNAME   H(*)   Name of remote port as aipsmtn:machine where n  */
/*                    is a number 1-NTapes and machine is the remote  */
/*                    machine name.  No imbedded blanks               */
/*                    ' ' => real tape                                */
/*  Output:                                                           */
/*     IERR    I      Error: 0 => okay                                */
/*  Unix Berkely version - bsd sockets.                               */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcberr, Z_mfcb;
   char device[10], lname[132];
   int i, fd, dupfd, optlen;
   char *machine_in;
   struct sockaddr_in client_in;
   struct servent *sp_in;
   struct hostent *hp_in;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   errno = 0;
                                        /* Extract Aips device name   */
                                        /* from pname                 */
   for (i=0; i < *plen && i < 131 && pname[i] !='\0' && pname[i] !=' ';
      i++)
      lname[i] = pname[i];
   lname[i] = '\0';
   for (i=0; i < 10 && lname[i] != ':' && lname[i] != '\0'; i++)
      device[i] = lname[i];
   device[i] = '\0';
   if (pname[i] == ':')
      machine_in = &lname[i+1];
   else {
      fprintf(stderr, "ZVTPO2: NO REMOTE MACHINE IN %s\n", pname);
      *ierr = 2;
      goto exit;
      }
                                        /* open socket  INET domain   */
   if (strncmp(device,"aipsmt",6)==0) {
                                        /* malformed names go here    */
      if (!isalnum(*machine_in)) {
         fprintf(stderr, "ZVTPO2: MALFORMED NAME %s\n", machine_in);
         fprintf(stderr, "ZVTPO2: FROM %s\n", lname);
         *ierr = 2;
         goto exit;
         }
                                        /* translate name             */
      if ((sp_in = getservbyname(device,"tcp")) == NULL) {
         fprintf(stderr,"ZVTPO2: tcp/%s not a service\n",device);
         *ierr = 2;
         goto exit;
         }
      if ((hp_in = gethostbyname(machine_in)) == NULL) {
         fprintf(stderr,"ZVTPO2: %s: unknown host\n",machine_in);
         *ierr = 2;
         goto exit;
         }
      memset ((char *) &client_in, 0, sizeof(client_in));
      memcpy ((char *) &client_in.sin_addr, hp_in->h_addr,
         hp_in->h_length);
      client_in.sin_family = hp_in->h_addrtype;
      client_in.sin_port = sp_in->s_port;
      if ((fd = (int) socket (AF_INET, SOCK_STREAM, 0)) < 0) {
         perror("ZVTPO2 socket (INET)");
         *ierr = 1;
         goto exit;
         }
      *(fcb + Z_fcbfd) = fd;
      if ((dupfd = dup(fd)) == -1) {
                                        /* Store 2nd buffer info in   */
                                        /* 1st buffer FCB for error   */
                                        /* processing (kludge).       */
         *(fcb + Z_mfcb + Z_fcbfd) = dupfd;
         for (i = 0; i < Z_mfcb; i++)
            *(fcb + i) = *(fcb + Z_mfcb + i);
         *(fcb + Z_fcberr) = errno;
         close (fd);
         *ierr = 6;
         goto exit;
         }
      else {
         *(fcb + Z_mfcb + Z_fcbfd) = dupfd;
         }

      if (connect(*fcb, (struct sockaddr *) &client_in,
         sizeof(client_in)) < 0) {
         perror("ZVTPO2 connect (INET)");
         *(fcb + Z_fcberr) = errno;
         close (fd);
         close (dupfd);
         *ierr = 1;
         goto exit;
         }
      optlen = sizeof (Z_sndbuf);
      if (getsockopt (fd, SOL_SOCKET, SO_SNDBUF, (char *) &Z_sndbuf,
         &optlen) < 0) {
         perror("ZVTPO2 SO_SNDBUF get");
         Z_sndbuf = 4096;
         }
/*    else
         fprintf (stderr,
            "Default SO_SNDBUF is %d bytes\n", Z_sndbuf); */
      optlen = sizeof (Z_rcvbuf);
      if (getsockopt (fd, SOL_SOCKET, SO_RCVBUF, (char *) &Z_rcvbuf,
         &optlen) < 0) {
         perror("ZVTPO2 SO_RCVBUF get");
         Z_rcvbuf = 4096;
         }
/*    else
         fprintf (stderr,
            "Default SO_RCVBUF is %d bytes\n", Z_rcvbuf); */
      Z_sndini = Z_sndbuf;
      Z_rcvini = Z_rcvbuf;
      }
                                        /* not aipsmtn                */
   else {
      fprintf(stderr, "ZVTPO2: NOT TAPE DEVICE = %s\n", device);
      *ierr = 2;
      }
                                        /* Put system error # in FTAB */
 exit:
   if (*ierr != 0) *(fcb + Z_fcberr) = errno;
   return;
}
