#include <stdio.h>
#include <errno.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>

zvtvo2_(fcb, pname, ierr)
/*--------------------------------------------------------------------*/
/*! open connection in client (virtual-TV) to server (remote, real-TV)*/
/*# TV-util                                                           */
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
/*  ZVTVO2 opens the connection in the Virtual TV program to the      */
/*  remote computer which provides the actual TV device.              */
/*  Inputs:                                                           */
/*     fcb     I(*)   File descriptor                                 */
/*     pname   C*(*)  Environment variable (logical) "TVDEVn"         */
/*                    identifies which TV is used and points to       */
/*                    further ID info - this is of the form           */
/*                    VTVIN:machine_name for a network connection.    */
/*  Output:                                                           */
/*     IERR    I      Error: 0 => okay                                */
/*                       1 => failure                                 */
/*                       2 => invalid device name                     */
/*   UNICOS version.                                                  */
/*--------------------------------------------------------------------*/
int *fcb, *ierr;
char pname[];
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcberr;
   extern int errno;
   char *device, shvar[10];
   int i;
   char *machine_in;
   struct sockaddr_in client_in;
   struct servent *sp_in;
   struct hostent *hp_in;
   extern char *getenv();
   extern char *index();
/*--------------------------------------------------------------------*/
   *ierr = 0;
   errno = 0;
                                        /* Extract logical device name*/
                                        /* from pname                 */
   for (i=0; i < 10 && pname[i] != ':' && pname[i] != '\0'; i++)
      shvar[i] = pname[i];
   shvar[i] = '\0';
                                        /* Get value for shell        */
                                        /* variable TVDEVn            */
   if (strncmp(shvar,"TVDEV",5) == 0) {
      if ((device = getenv(shvar)) == NULL) {
                                        /* TVDEVn undefined           */
         fprintf(stderr, "ZVTVO2: NO SUCH DEVICE = %s\n",shvar);
         *ierr = 2;
         goto exit;
         }
                                        /* Otherwise, open socket     */
                                        /* INET domain                */
      if (strncmp(device,"VTVIN",5)==0) {
         if(( machine_in=(char *)index(device,':')) == NULL) {
            fprintf(stderr, "ZVTVO2: NO REMOTE MACHINE IN %s\n", shvar);
            *ierr = 2;
            goto exit;
            }
         else
            machine_in++;
                                        /* malformed names go here    */
         if (!isalnum(*machine_in)) {
            fprintf(stderr, "ZVTVO2: MALFORMED NAME %s\n", machine_in);
            *ierr = 2;
            goto exit;
            }
                                        /* translate name             */
         if ((sp_in = getservbyname("VTVIN","tcp")) == NULL) {
            fprintf(stderr,"ZVTVO2: tcp/VTVIN not a service\n");
            *ierr = 2;
            goto exit;
            }
         if ((hp_in = gethostbyname(machine_in)) == NULL) {
            fprintf(stderr,"ZVTVO2: %s: unknown host\n",machine_in);
            *ierr = 2;
            goto exit;
            }
         bzero((char *) &client_in, sizeof(client_in));
         bcopy(hp_in->h_addr, (char *) &client_in.sin_addr,
            hp_in->h_length);
         client_in.sin_family = hp_in->h_addrtype;
         client_in.sin_port = sp_in->s_port;
         if ((*fcb = (int)socket(AF_INET, SOCK_STREAM, 0)) < 0) {
            perror("ZVTVO2 socket (INET)");
            *ierr = 1;
            }
         if (connect(*fcb, (struct sockaddr *) &client_in,
                     sizeof(client_in)) < 0) {
            perror("ZVTVO2 connect (INET)");
            *ierr = 1;
            }
         }
                                        /* UNIX domain not okay here  */
      else {
         fprintf(stderr,"ZVTVO2: UNIX domain not supported\n");
         *ierr = 2;
         }
      }
                                        /* not TVDEVn                 */
   else {
      fprintf(stderr, "ZVTVO2: NOT TV DEVICE = %s\n", shvar);
      *ierr = 2;
      }
                                        /* Put system error # in FTAB */
 exit:
   if (*ierr != 0) *(fcb + Z_fcberr) = errno;
   return;
}
