#define Z_ssso2__
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <stdlib.h>
#include <ctype.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <netdb.h>


int buffered, Z_maxseg;
#define PACKSIZE (4096/sizeof(short int)+6)
   short int packet_int[PACKSIZE];
   int pack_offset;
#define TRUE 1
#define NUMOP 83       /* Largest opcode                              */
int bufferop[NUMOP+1]; /* bufferop[OPCODE] >0 --> no status return    */
                       /* if buffered                                 */
#define OPEN    11     /* Opens the SSS connection                    */
#define CLOSE   12     /* Close the SSS, allows new connections       */
#define INTGT   13     /* Interrogate: get SSS status, not impl.      */
#define WINDO   14     /* Read, write the X window size               */
#define CLEAR   15     /* Clear some or all channels                  */
#define VIEW    16     /* Do/hold screen updates                      */
#define IMWRT   21     /* Write image line to some channel            */
#define IMRD    22     /* Read image line from some channel           */
#define WLUT    41     /* Write LUT to a channel.                     */
#define RLUT    42     /* Read LUT to a channel.                      */
#define WOFM    43     /* Write OFM.                                  */
#define ROFM    44     /* Read OFM.                                   */
#define GRAPH   45     /* On/off graphics channel(s)                  */
#define SPLIT   46     /* On/off image channels(s)                    */
#define WGRFX   51     /* Write graphics/cursor colours               */
#define RGRFX   52     /* Read  graphics/cursor colours               */
#define RCURS   61     /* Read the cursor position.                   */
#define RBUTT   62     /* Read the status of the buttons              */
#define WCURS   63     /* Write the cursor position.                  */
#define RCURB   64     /* Read the cursor position and buttons        */
#define WZOOM   81     /* Write zoom info to the SSS                  */
#define WSCROL  82     /* Write scroll registers                      */
#define WZSCR   83     /* Write zoom/scroll to SSS using ULC          */

#if __STDC__
   void zssso2_ (int *fcb, char pname[48], int *ierr)
#else
   zssso2_ (fcb, pname, ierr)
   int *fcb, *ierr;
   char pname[48];
#endif
/*--------------------------------------------------------------------*/
/*! Open SUN-Screen-Server device, set-up any buffer                  */
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
/*  Assign an i/o channel (i.e., open) for SSS TV device.  Uses BSD   */
/*  sockets                                                           */
/*  Inputs:                                                           */
/*     pname  C*48  'logical' name of TV device. Contains a pathname  */
/*                  for Unix domain sockets or a name of the form     */
/*                  SSSIN:machine_name for Internet domain sockets.   */
/*                  Must begin with the string "TVDEV".               */
/*  Outputs:                                                          */
/*     fcb    I(*)  File descriptor (part of FTAB).                   */
/*     ierr   I     Error code: 0 => success                          */
/*                     1 => failure                                   */
/*                     2 => invalid device name                       */
/*  SUN - Berkeley 4.2 UNIX version.                                  */
/*--------------------------------------------------------------------*/
{
   char *device, shvar[10];
   int i, optlen;
   char *machine_in;
   struct sockaddr_un client;
   struct sockaddr_in client_in;
   struct servent *sp_in;
   struct hostent *hp_in;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   errno = 0;
   buffered = 0;
   pack_offset = 0;
                                        /* Extract logical device name*/
   for (i=0; i < 10 && pname[i] != ':' && pname[i] != '\0'; i++)
      shvar[i] = pname[i];
   shvar[i] = '\0';
                                        /* Get value for TVDEVn       */
   if (strncmp(shvar,"TVDEV",5) == 0) {
      if ((device = getenv(shvar)) == NULL) {
                                        /* undefined                  */
         fprintf(stderr, "ZSSSO2: NO SUCH DEVICE = %s\n",shvar);
         *ierr = 2;
         goto exit;
         }
                                        /* open socket, INET domain   */
      if (strncmp(device,"SSSIN",5) == 0) {
         if (strncmp(device,"SSSINB",6) == 0) buffered = 1;
         if (( machine_in = strchr (device,':')) == NULL) {
            fprintf(stderr, "ZSSSO2: NO REMOTE MACHINE SPECIFIED IN %s\n",
                   shvar);
            *ierr = 2;
            goto exit;
            }
         else
            machine_in++;
                                        /* malformed names go here    */
         if (!isalnum(*machine_in)) {
            fprintf(stderr, "ZSSSO2: MALFORMED NAME = %s\n", machine_in);
            *ierr = 2;
            goto exit;
            }
 
         if ((sp_in = getservbyname("SSSIN","tcp")) == NULL) {
            fprintf(stderr,"ZSSSO2: tcp/SSSIN NOT A SERVICE\n");
            *ierr = 2;
            goto exit;
            }
         if (( hp_in = gethostbyname(machine_in)) == NULL) {
            fprintf(stderr,"ZSSSO2: %s: UNKNOWN HOST\n",machine_in);
            *ierr = 2;
            goto exit;
            }
         memset ((char *)&client_in, 0, sizeof(client_in));
         memcpy ((char *)&client_in.sin_addr, hp_in->h_addr,
            hp_in->h_length);
         client_in.sin_family = hp_in->h_addrtype;
         client_in.sin_port = sp_in->s_port;
         if ((*fcb = (int)socket(AF_INET, SOCK_STREAM, 0)) < 0) {
            perror("ZSSSO2 socket (INET)");
            *ierr = 1;
            }
         if (connect(*fcb, (struct sockaddr *) &client_in,
             sizeof(client_in)) < 0){
            perror("ZSSSO2 connect (INET)");
            *ierr = 1;
            }
         }
                                        /* open socket, UNIX domain   */
      else {
         if ((*fcb = (int)socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
            perror("ZSSSO2 socket (UNIX)");
            *ierr = 1;
            }
         client.sun_family = AF_UNIX;
         strcpy(client.sun_path, device);

         if (connect((*fcb), (struct sockaddr *) &client,
/* does not work??? (sizeof(client.sun_path) + sizeof(client.sun_family))) */
                     (4 + 4))
            < 0) {
            perror("ZSSSO2 connect (UNIX)");
            *ierr = 1;
            }
         }
      if (*ierr == 0) {
         optlen = sizeof (Z_maxseg);
         if (getsockopt (*fcb, IPPROTO_TCP, TCP_MAXSEG, (char *) &Z_maxseg,
            &optlen) < 0) {
            perror("ZVTPO2 TCP_MAXSEG get");
            Z_maxseg = 1024;
            }
         }
      }
   else {
      fprintf(stderr, "ZSSSO2: NO SUCH DEVICE = %s\n",shvar);
      *ierr = 2;
      }
                                        /* system error in FTAB      */
exit:
   if (*ierr != 0) *(fcb + 2) = errno;
   if (!buffered) {
      for (i=0; i<(NUMOP+1); i++) bufferop[i]=0;
      }
   else {
      for (i=0; i<(NUMOP+1); i++) bufferop[i]=0;
      bufferop[CLEAR]=TRUE;
      bufferop[VIEW]=TRUE;
      bufferop[IMWRT]=TRUE;
      bufferop[WLUT]=TRUE;
      bufferop[WOFM]=TRUE;
      bufferop[WCURS]=TRUE;
      bufferop[GRAPH]=TRUE;
      bufferop[SPLIT]=TRUE;
      bufferop[WGRFX]=TRUE;
      bufferop[WZOOM]=TRUE;
      bufferop[WSCROL]=TRUE;
      bufferop[WZSCR]=TRUE;
      }

   return;
}
