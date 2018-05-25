#define Z_ssso2__
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/stat.h>
#include <ctype.h>
#include <AIPSPROTO.H>

   int Z_sndbufs, Z_rcvbufs, Z_sndinis, Z_rcvinis;

int SSSremember=0;
struct sockaddr_un SSS_sock_un;
struct sockaddr_in SSS_sock_in;

int buffered;
#define PACKSIZE (65536+6)
   short int packet_int[PACKSIZE];
   int pack_offset;
#define TRUE 1

#define NUMOP 83       /* Largest opcode                              */
int bufferop[NUMOP+1]; /* bufferop[OPCODE] >0 --> no status return    */
                       /* if buffered                                 */
#define INIT    29     /* init the TV                                 */
#define INTGT   10     /* Interrogate: get XAS major parameters       */
#define OPEN    11     /* Opens the XAS connection                    */
#define CLOSE   12     /* Close the XAS, allows new connections       */
#define INTGTO  13     /* Interrogate: get XAS old major parameters   */
#define WINDO   14     /* Read, write the X window size               */
#define INITO   15     /* init the TV old form                        */
#define VIEW    16     /* Do/hold screen updates                      */
#define PSAVE   17     /* save DTVC.INC parameters                    */
#define XDIE    18     /* close down XAS and lock servers             */
#define IMWRT   19     /* Write image line to some channel            */
#define IMRD    20     /* Read image line from some channel           */
#define OIMWRT  21     /* Write image line to some channel OLD        */
#define OIMRD   22     /* Read image line from some channel OLD       */
#define OFILL   23     /* fill area in some or all channels           */
#define CLEAR   24     /* Clear some or all channels                  */
#define OVECT   25     /* connect two points with line                */
#define CHARS   26     /* write character string                      */
#define FILL    27     /* fill area in some or all channels           */
#define VECT    28     /* connect two points with line                */
#define CATIN   31     /* Init image catalog                          */
#define CATRD   32     /* Read image catalog                          */
#define CATWR   33     /* Write image catalog                         */
#define CATOV   34     /* Use image catalog - covered?                */
#define CATFI   35     /* Find from image catalog                     */
#define WOFM    37     /* Write OFM.                                  */
#define ROFM    38     /* Read OFM.                                   */
#define WLUT    39     /* Write LUT to a channel.                     */
#define RLUT    40     /* Read LUT to a channel.                      */
#define WLOT    41     /* Write LUT to a channel old                  */
#define RLOT    42     /* Read LUT to a channel old                   */
#define WOFMO   43     /* Write OFM old form                          */
#define ROFMO   44     /* Read OFM old form                           */
#define GRAPH   45     /* On/off graphics channel(s)                  */
#define SPLOT   46     /* On/off image channels(s) - obsolete         */
#define SPLAT   47     /* On/off image channels(s) - replace 2/08     */
#define SPLIT   48     /* On/off image channels(s)                    */
#define WGRFX   51     /* Write graphics/cursor colours               */
#define RGRFX   52     /* Read  graphics/cursor colours               */
#define RCURS   61     /* Read the cursor position.                   */
#define RBUTT   62     /* Read the status of the buttons              */
#define WCURS   63     /* Write the cursor position.                  */
#define RCURB   64     /* Read the cursor position and buttons        */
#define WZSCR   83     /* Write zoom/scroll to XAS using ULC          */

#if __STDC__
   void zssso2_ (int *fcb, char pname[48], int *ierr)
#else
   void zssso2_ (fcb, pname, ierr)
   int *fcb, *ierr;
   char pname[48];
#endif
/*--------------------------------------------------------------------*/
/*! Open SUN-Screen-Server device, set-up any buffer                  */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1999, 2004, 2008                              */
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
/*  Assign an i/o channel (i.e., open) for SSS TV device.  Uses BSD   */
/*  internet or UNIX sockets                                          */
/*  Inputs:                                                           */
/*     pname  C*48  'logical' name of TV device. Contains a pathname  */
/*                  for Unix domain sockets or a name of the form     */
/*                  sssin:machine_name for Internet domain sockets.   */
/*                  Must begin with the string "TVDEV".               */
/*  Outputs:                                                          */
/*     fcb    I(*)  File descriptor (part of FTAB).                   */
/*     ierr   I     Error code: 0 => success                          */
/*                     1 => failure                                   */
/*                     2 => invalid device name                       */
/*  SUN - Berkeley 4.2 UNIX version.                                  */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcberr;

   char *device, shvar[10];
   int i, optlen, s, isspec, n7 = 7;
   char *machine_in, msg[80];
   struct servent *sp;
   struct hostent *hp;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   errno = 0;
   pack_offset = 0;
                                        /* Extract logical device name*/
   for (i=0; i < 10 && pname[i] != ':' && pname[i] != '\0'; i++)
      shvar[i] = pname[i];
   shvar[i] = '\0';
                                        /* Get value for TVDEVn       */
   if (SSSremember == 0) {
      if (strncmp(shvar,"TVDEV",5) == 0) {
         if ((device = getenv(shvar)) == NULL) {
                                        /* undefined                  */
            sprintf(msg, "ZSSSO2: NO SUCH DEVICE = %s\n",shvar);
            zmsgwr_ (msg, &n7);
            *ierr = 2;
            }
         }
      else {
         sprintf(msg, "ZSSSO2: NO SUCH DEVICE = %s\n",shvar);
         zmsgwr_ (msg, &n7);
         *ierr = 2;
         }
      }
   if (*ierr == 0) {
      if (SSSremember == 0) {
         isspec = 2;
         if (strncmp(device,"sssin",5) == 0) isspec = 1;
                                        /* open socket, INET domain   */
                                        /* open socket, INET domain   */
         if (strncmp(device,"sssinb",6) == 0) buffered = 1;
         if (isspec == 1) {
            if (( machine_in = strchr (device,':')) == NULL) {
              sprintf (msg,
                  "ZSSSO2: NO REMOTE MACHINE SPECIFIED IN %s\n", shvar);
               zmsgwr_ (msg, &n7);
               *ierr = 2;
               }
            else {
               machine_in++;
                                        /* malformed names go here    */
               if (!isalnum(*machine_in)) {
                  sprintf (msg, "ZSSSO2: MALFORMED NAME = %s\n",
                     machine_in);
                  zmsgwr_ (msg, &n7);
                  *ierr = 2;
                  }
               else if ((sp = getservbyname("sssin","tcp")) == NULL) {
                  sprintf (msg,"ZSSSO2: tcp/sssin NOT A SERVICE\n");
                  zmsgwr_ (msg, &n7);
                  *ierr = 2;
                  }
               else if (( hp = gethostbyname(machine_in)) == NULL) {
                  sprintf (msg, "ZSSSO2: %s: UNKNOWN HOST\n",
                     machine_in);
                  zmsgwr_ (msg, &n7);
                  *ierr = 2;
                  }
               else {
                  memset ((char *)&SSS_sock_in, 0, sizeof(SSS_sock_in));
                  memcpy ((char *)&SSS_sock_in.sin_addr,
                     (char *)hp->h_addr, hp->h_length);
                  SSS_sock_in.sin_family = hp->h_addrtype;
                  SSS_sock_in.sin_port = sp->s_port;
                  }
               }
            }
                                        /* UNIX domain   */
         else {
            SSS_sock_un.sun_family = AF_UNIX;
            strcpy(SSS_sock_un.sun_path, device);
            }
         SSSremember = isspec;
         }
      }
                                        /* open socket                */
   if (*ierr == 0) {
                                        /* INET domain                */
      if (SSSremember == 1) {
         if ((s = (int)socket(AF_INET, SOCK_STREAM, 0)) < 0) {
            sprintf (msg, "ZSSSO2 socket (INET) %s", strerror(errno));
            zmsgwr_ (msg, &n7);
            *ierr = 1;
            }
         else if (connect(s, (struct sockaddr *) &SSS_sock_in,
            sizeof(SSS_sock_in)) < 0){
            sprintf (msg, "ZSSSO2 connect (INET) %s", strerror(errno));
            zmsgwr_ (msg, &n7);
            *ierr = 1;
            *(fcb + Z_fcberr) = errno;
            close (s);
            *(fcb + Z_fcbfd) = 0;
            }
         else {
            *(fcb + Z_fcbfd) = s;
            *(fcb + Z_fcberr) = 0;
            }
         }
                                        /* UNIX domain                */
      else {
         if ((s = (int)socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
            sprintf (msg, "ZSSSO2 socket (UNIX) %s", strerror(errno));
            zmsgwr_ (msg, &n7);
            *ierr = 1;
            }
         else if (connect(s, (struct sockaddr *) &SSS_sock_un,
            (sizeof(SSS_sock_un.sun_path)
            +sizeof(SSS_sock_un.sun_family))) < 0) {
            sprintf (msg, "ZSSSO2 connect (UNIX), %s", strerror(errno));
            zmsgwr_ (msg, &n7);
            *ierr = 1;
            *(fcb + Z_fcberr) = errno;
            close(s);
            *(fcb + Z_fcbfd) = 0;
            }
         else {
            *(fcb + Z_fcbfd) = s;
            *(fcb + Z_fcberr) = 0;
            }
         }
      }
                                        /* send data                  */
   if (*ierr == 0) {
      optlen = sizeof (Z_sndbufs);
      if (getsockopt (*fcb, SOL_SOCKET, SO_SNDBUF, (char *) &Z_sndbufs,
         &optlen) < 0) {
         sprintf (msg, "ZSSSO2 SO_SNDBUF get %s", strerror(errno));
         zmsgwr_ (msg, &n7);
         Z_sndbufs = 4096;
         }
      optlen = sizeof (Z_rcvbufs);
      if (getsockopt (*fcb, SOL_SOCKET, SO_RCVBUF, (char *) &Z_rcvbufs,
         &optlen) < 0) {
         sprintf (msg, "ZSSSO2 SO_RCVBUF get %s", strerror(errno));
         zmsgwr_ (msg, &n7);
         Z_rcvbufs = 4096;
         }
      Z_sndinis = Z_sndbufs;
      Z_rcvinis = Z_rcvbufs;
   }
                                        /* system error in FTAB      */
exit:
   if (*ierr != 0) SSSremember = 0;
   if (!buffered) {
      for (i=0; i<(NUMOP+1); i++) bufferop[i]=0;
      }
   else {
      for (i=0; i<(NUMOP+1); i++) bufferop[i]=0;
      bufferop[INIT]=TRUE;
      bufferop[INITO]=TRUE;
      bufferop[FILL]=TRUE;
      bufferop[CLEAR]=TRUE;
      bufferop[VECT]=TRUE;
      bufferop[PSAVE]=TRUE;
      bufferop[CHARS]=TRUE;
      bufferop[VIEW]=TRUE;
      bufferop[IMWRT]=TRUE;
      bufferop[OIMWRT]=TRUE;
      bufferop[WLUT]=TRUE;
      bufferop[WLOT]=TRUE;
      bufferop[WOFM]=TRUE;
      bufferop[WOFMO]=TRUE;
      bufferop[WCURS]=TRUE;
      bufferop[GRAPH]=TRUE;
      bufferop[SPLOT]=TRUE;
      bufferop[SPLAT]=TRUE;              /* but not SPLIT */
      bufferop[WGRFX]=TRUE;
      bufferop[WZSCR]=TRUE;
      bufferop[CATIN]=TRUE;
      bufferop[CATWR]=TRUE;
      }

   return;
}
