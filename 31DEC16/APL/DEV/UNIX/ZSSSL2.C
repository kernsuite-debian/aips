#define Z_sssl2__
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
#include <string.h>
#include <ctype.h>
#include <AIPSPROTO.H>

int TVLremember=0;
struct sockaddr_un TVL_sock_un;
struct sockaddr_in TVL_sock_in;

#if __STDC__
   void zsssl2_ (int *fcb, char pname[48], int *lock, int *ierr)
#else
   void zsssl2_ (fcb, pname, lock, ierr)
   int *fcb, *lock, *ierr;
   char pname[48];
#endif
/*--------------------------------------------------------------------*/
/*! Lock/unlock SUN-Screen-Server device                              */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1997, 1999, 2008                                   */
/*;  Associated Universities, Inc. Washington DC, USA.                */
/*;  Eric W. Greisen                                                  */
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
/*  Assign an i/o channel for TVlock device, send lock/unlock command */
/*  and then deassign the channel.  Uses BSD sockets                  */
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
/*                     3 => I/O error                                 */
/*                     4 => Locked by someone else                    */
/*  SUN - Berkeley 4.2 UNIX version.                                  */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcberr;

   char *device, shvar[10], data[3];
   int i, optlen, mbytes, lbytes, s, isspec, n7 = 7;
   char *machine_in, msg[80];
   struct servent *sp;
   struct hostent *hp;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   errno = 0;
                                        /* Extract logical device name*/
   for (i=0; i < 10 && pname[i] != ':' && pname[i] != '\0'; i++)
      shvar[i] = pname[i];
   shvar[i] = '\0';
                                        /* Value for TVDEVn/TVLCKn    */
   if (TVLremember == 0) {
      if (strncmp(shvar,"TVDEV",5) == 0) {
                                        /* definition                 */
         if ((device = getenv(shvar)) == NULL) {
            sprintf (msg, "ZSSSL2: NO SUCH DEVICE = %s\n", shvar);
            zmsgwr_ (msg, &n7);
            *ierr = 2;
            }
         }
      else {
         sprintf (msg, "ZSSSL2: NO SUCH DEVICE = %s\n", shvar);
            zmsgwr_ (msg, &n7);
         *ierr = 2;
         }
      }
   if (*ierr == 0) {
      if (TVLremember == 0) {
         isspec = 2;
         if (strncmp(device,"sssin",5) == 0) isspec = 1;
                                        /* open socket, INET domain   */
         if (isspec == 1) {
           if ((machine_in = strchr (device,':')) == NULL) {
              sprintf (msg,
                "ZSSSL2: NO REMOTE MACHINE SPECIFIED IN %s\n",shvar);
               zmsgwr_ (msg, &n7);
               *ierr = 2;
               goto exit;
               }
            else {
               machine_in++;
                                        /* malformed names go here    */
               if (!isalnum(*machine_in)) {
                  sprintf (msg, "ZSSSL2: MALFORMED NAME = %s\n",
                     machine_in);
                  zmsgwr_ (msg, &n7);
                  *ierr = 2;
                  }
               else if ((hp = gethostbyname(machine_in)) == NULL) {
                  sprintf (msg, "ZSSSL2: %s: UNKNOWN HOST\n",
                     machine_in);
                  zmsgwr_ (msg, &n7);
                  *ierr = 2;
                  }
               else if ((sp = getservbyname("ssslock","tcp")) == NULL) {
                  sprintf (msg, "ZSSSL2: tcp/ssslock NOT A SERVICE\n");
                  zmsgwr_ (msg, &n7);
                  *ierr = 2;
                  }
               else {
                  memset ((char *)&TVL_sock_in, 0, sizeof(TVL_sock_in));
                  memcpy ((char *)&TVL_sock_in.sin_addr,
                     (char *)hp->h_addr, hp->h_length);
                  TVL_sock_in.sin_family = hp->h_addrtype;
                  TVL_sock_in.sin_port = sp->s_port;
                  }
               }
            }
                                        /* UNIX domain   */
         else {
            if ((device = getenv(getenv("TVLOK"))) == NULL) {
                                        /* undefined                  */
               sprintf (msg, "ZSSSL2: NO SUCH DEVICE = TVLOK\n");
               zmsgwr_ (msg, &n7);
               *ierr = 2;
               }
            TVL_sock_un.sun_family = AF_UNIX;
            strcpy(TVL_sock_un.sun_path, device);
            }
         TVLremember = isspec;
         }
      }
                                        /* open socket                */
   if (*ierr == 0) {
                                        /* INET domain                */
      if (TVLremember == 1) {
         if ((s = (int)socket(TVL_sock_in.sin_family, SOCK_STREAM, 0))
            < 0) {
            sprintf (msg, "ZSSSL2 socket (INET) %s", strerror(errno));
            zmsgwr_ (msg, &n7);
            *ierr = 1;
             }
         else if (connect(s, (struct sockaddr *) &TVL_sock_in,
            sizeof(TVL_sock_in)) < 0){
            sprintf (msg, "ZSSSL2 connect (INET) %s", strerror(errno));
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
            sprintf (msg, "ZSSSL2 socket (UNIX) %s", strerror(errno));
            zmsgwr_ (msg, &n7);
            *ierr = 1;
            }
         else if (connect((s), (struct sockaddr *) &TVL_sock_un,
            (sizeof(TVL_sock_un.sun_path)
            +sizeof(TVL_sock_un.sun_family))) < 0) {
            sprintf (msg, "ZSSSL2 connect (UNIX) %s", strerror(errno));
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
      data[0] = '1';
      if (*lock > 0)
         data[1] = 'L';
      else
         data[1] = 'U';
      data[2] = '\0';

      lbytes = 2;
      if ((mbytes = write (s, data, lbytes)) != lbytes) {
         sprintf (msg, "ZSSSL2 write data %s", strerror(errno));
         zmsgwr_ (msg, &n7);
         *ierr = 3;
         *(fcb + Z_fcberr) = errno;
         }
                                        /* answer                     */
      else if ((mbytes = read (s, data, lbytes)) != lbytes) {
         sprintf (msg, "ZSSSL2 read answer %s", strerror(errno));
         zmsgwr_ (msg, &n7);
         *ierr = 3;
         *(fcb + Z_fcberr) = errno;
         }
      if ((*lock > 0) && (data[0] == 'N')) *ierr = 4;
      if (close(s) != 0) {
         sprintf (msg, "ZSSSL2 close socket %s", strerror(errno));
         zmsgwr_ (msg, &n7);
         *ierr = 3;
         *(fcb + Z_fcberr) = errno;
         }
      }
                                        /* system error in FTAB      */
exit:
   if (*ierr != 0) TVLremember = 0;

   return;
}
