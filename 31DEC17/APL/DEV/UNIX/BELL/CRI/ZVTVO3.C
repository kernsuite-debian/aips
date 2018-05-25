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

zvtvo3_(fcb, ierr)
/*--------------------------------------------------------------------*/
/*! open connection in server (real-TV) to client (virtual-TV)        */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
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
/*--------------------------------------------------------------------*/
/*  ZVTVO3 opens the connection in TVMON to the remote machine which  */
/*  is running the AIPS VTV (Virtual TV) code.                        */
/*  Outputs:                                                          */
/*     fcb   I(*)   File descriptor                                   */
/*     ierr  I      Error: 0 => okay                                  */
/*                     1 => failure                                   */
/*                     2 => invalid device name                       */
/*  UNICOS version.                                                   */
/*  Create a socket to receive commands/requests from remote          */
/*  computers for TV service.                                         */
/*--------------------------------------------------------------------*/
int *fcb, *ierr;
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
   extern int errno;
   char *device, shvar[10];
   int len, sfd, cfd;
   char *machine_in;
   extern char *getenv();
   extern char *index();
   extern char *strncpy();
/*--------------------------------------------------------------------*/
   *ierr = 0;
   errno = 0;

   strncpy(shvar, "VTVDEV1", 9);
/*                                      /* Get value for shell        */
/*                                      /* variable for VTVDEV        */
   serverb.sun_family = AF_UNIX;
   if ((device = getenv(shvar)) == NULL) {
                                        /* VTVDEV is undefined        */
      fprintf(stderr, "ZVTVO3: NO SUCH DEVICE = %s\n",shvar);
      *ierr = 2;
      goto exit;
      }
                                        /* open socket, INET domain   */
   if (strncmp(device,"VTVIN",5)==0) {
      if(( machine_in=(char *)index(device,':')) == NULL) {
         fprintf(stderr, "ZVTVO3: NO REMOTE MACHINE IN %s\n", shvar);
         *ierr = 2;
         goto exit;
         }
      else
         machine_in++;
                                        /* malformed names go here    */
      if (!isalnum(*machine_in)) {
         fprintf(stderr, "ZVTVO3: MALFORMED NAME %s\n", machine_in);
         *ierr = 2;
         goto exit;
         }
                                        /* translate name             */
      if ((spb_in = getservbyname("VTVIN","tcp")) == NULL) {
         fprintf(stderr,"ZVTVO3: tcp/VTVIN not a service\n");
         *ierr = 2;
         goto exit;
         }
      serverb_in.sin_port = spb_in->s_port;
      if ((sfd = (int)socket(AF_INET, SOCK_STREAM, 0)) < 0) {
         perror("ZVTVO3 socket (INET)");
         *ierr = 1;
         }
      *(fcb+Z_fcbfd) = sfd;
      if (bind(sfd, (struct sockaddr *) &serverb_in, sizeof(serverb_in)) < 0) {
         perror("ZVTVO3 bind (INET)");
         *ierr = 1;
         }
      listen(sfd, 5);
      len = sizeof(fromb_in);
      if ((cfd = accept(sfd, (struct sockaddr *) &fromb_in, &len)) < 0) {
         perror("ZVTVO3 accept (INET)");
         *ierr = 1;
         }
      else {
         *(fcb+Z_fcbsp1) = cfd;
         }
      }
                                        /* UNIX domain not in VTV     */
   else {
      fprintf(stderr,"ZVTVO3: UNIX domain not supported\n");
      *ierr = 2;
      }
                                        /* system error # in FTAB     */
 exit:
   if (*ierr != 0) *(fcb + Z_fcberr) = errno;
   return;
}
