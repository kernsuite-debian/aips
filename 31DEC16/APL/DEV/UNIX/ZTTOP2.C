#define Z_ttop2__
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
                                        /* Global data for remembered */
                                        /* translation and flag       */
char pathname2[MAXAPATH];
int remembered=0;
struct sockaddr_un mc_sock_nune;
struct sockaddr_in ms_sock_name;
int msg_fd = -1;

#if __STDC__
   void zttop2_(int *fcb, char pname[MAXPNAME], int *ierr)
#else
   void zttop2_(fcb, pname, ierr)
   int *fcb, *ierr;
   char pname[MAXPNAME];
#endif
/*--------------------------------------------------------------------*/
/*! read/write from/to a message terminal                             */
/*# Graphics                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1998, 2000                                    */
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
/*  Open a message terminal for exclusive use.                        */
/*                                                                    */
/*  Inputs:                                                           */
/*     pname   C*48   A "logical", either TTDEVnn or TASKTTn, which   */
/*                      in turn translates to a hostname or hardwired */
/*                      terminal device.  If translation cannot be    */
/*                      accessed as a file, a socket is assumed.      */
/*  Output:                                                           */
/*     fcb     I(*)   File control block for opened device            */
/*     ierr    I      Error return code: 0 => no error                */
/*                             2 => no such logical device            */
/*                             3 => invalid hostname or service       */
/*                             6 => socket or connect call failed     */
/*  Generic UNIX version -- assumes berkeley sockets.                 */
/*  PPM 93/Mar/03: Pat Murphy, NRAO.  Clone of ztkop2, originally     */
/*                 by Jonathan D. Eisenhamer, UCLA.                   */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4, Z_nfcbfd, Z_nfcber;

   int llen, xlen, tlen, xlnb, jerr, fd, isspec;
   register int i;
   char lognam[MAXPNAME],
        pathname[MAXAPATH],
        lnam[6], tlnam[8], *device;
                                        /* Hostname, inet srvc info   */
   struct hostent *hp;
   struct servent *sp;
                                        /* Socket descriptor, name    */
   int s;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Extract logical device     */
                                        /* name.                      */
   for (i = 0; i < MAXPNAME && pname[i] != ':' && pname[i] != ' '; i++)
      lognam[i] = pname[i];
   lognam[i] = '\0';
                                        /* Translate logical device   */
                                        /* name and null terminate.   */
   jerr = 0;
   if (remembered == 0) {
      llen = MAXPNAME;
      xlen = MAXAPATH;
      ztrlo2_ (&llen, lognam, &xlen, pathname, &xlnb, &jerr);
      if (jerr != 0) {
         fprintf (stderr, "ZTTOP2: error translating %s\n", lognam);
         *ierr = 2;
         }
      else {
         if ((device = getenv(getenv("TVDEV"))) == NULL) {
                                        /* undefined                  */
            fprintf (stderr, "ZTTOP2: NO SUCH DEVICE = TVDEV\n");
            *ierr = 2;
            jerr = 2;
            }
         }
      }
                                        /* Only do if ztrlo2 worked.. */
   if (jerr == 0) {
                                        /* and if we haven't before.. */
      if (remembered == 0) {
         pathname[xlnb] = '\0';
                                        /* open socket, INET domain   */
         isspec = 2;
         if (strncmp(device,"sssin",5) == 0) isspec = 1;
                                        /* Now, if it's NOT special,  */
                                        /* assume that it is meant to */
                                        /* be an INET domain socket   */
         if (isspec == 1) {
            if ((hp = gethostbyname (pathname)) == 0) {
                                        /* ouch, something is wrong   */
               *(fcb + Z_nfcber) = errno;
               *ierr = 3;
               }
            else if ((sp = getservbyname ("msgserv", "tcp")) == 0) {
                                        /* Get the port on which the  */
                                        /* the msgserver resides.     */
               *(fcb + Z_nfcber) = errno;
               *ierr = 3;
               }
            else {
                                        /* ok, NOW remember this!     */
               strcpy (pathname2, pathname);
               remembered = isspec;
               memset ((char *)&ms_sock_name, 0, sizeof (ms_sock_name));
               memcpy ((char *)&ms_sock_name.sin_addr,
                       (char *)hp->h_addr, hp->h_length);
               ms_sock_name.sin_family = hp->h_addrtype;
               ms_sock_name.sin_port   = sp->s_port;
               }
            }
         else {                      /* INET domain */
            if ((device = getenv(getenv("TTDEV"))) == NULL) {
                                        /* undefined                  */
               fprintf(stderr, "ZTTOP22: NO SUCH DEVICE = TTDEV\n");
               *ierr = 2;
               }
            mc_sock_nune.sun_family = AF_UNIX;
            strcpy(mc_sock_nune.sun_path, device);
            remembered = isspec;
            }
         }
      if (*ierr == 0) {
                                        /* Create the communications  */
                                        /* socket.                    */
         if (remembered == 1) {
            if ((s = socket (ms_sock_name.sin_family, SOCK_STREAM, 0))
                == -1) {
               *(fcb + Z_fcberr) = errno;
               *ierr = 6;
               }
                                        /* Connect to the server.     */
                                        /* From here on, everything   */
                                        /* is treated the same.  Only */
                                        /* thing to watch is how to   */
                                        /* close things (zttcl2)      */
                                        /* Note non-fcp I/O here!!!   */
            else {
               if (connect (s,  (struct sockaddr *) &ms_sock_name,
                  sizeof(ms_sock_name)) == -1) {
                  *(fcb + Z_nfcber) = errno;
                  *ierr = 6;
                  close(s);
                  *(fcb + Z_nfcbfd) = 0;
                  }
               else {
                  *(fcb + Z_nfcbfd) = s;
                  *(fcb + Z_nfcber) = 0;
                  }
               }
            }
         else {
            if ((s = (int)socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
               perror("TTOP2 socket (UNIX)");
               *(fcb + Z_nfcber) = errno;
               *ierr = 1;
               }
            else {
               if (connect(s, (struct sockaddr *) &mc_sock_nune,
                  (sizeof(mc_sock_nune.sun_path)
                  +sizeof(mc_sock_nune.sun_family))) < 0) {
/*                perror("TTOP2 connect (UNIX)");  */
                  *(fcb + Z_nfcber) = errno;
                  *ierr = 6;
                  close(s);
                  *(fcb + Z_nfcbfd) = 0;
                  }
               else {
                  *(fcb + Z_nfcbfd) = s;
                  *(fcb + Z_nfcber) = 0;
                  }
               }
            }
         }
      }
                                        /* something went amiss, so   */
                                        /* reset TTDEV and quit       */
   if (*ierr != 0) {
      remembered = 0;
      strcpy(lnam, "TTDEV");
      strcpy(tlnam, "TTDEV00");
      llen=strlen(lnam);
      tlen=strlen(tlnam);
      zcrlog_(&llen, lnam, &tlen, tlnam, &jerr);
      if (jerr != 0) {
         fprintf (stderr,
                "ZTTOP2: (info) problem opening socket to msgserver\n");
         fprintf (stderr,
                "ZTTOP2: (info) failed to reset message device\n");
         }
      }

                                       /* store the file descriptor  */
                                       /* so that ZTACV9 can get at  */
                                       /* it                         */
   msg_fd = *(fcb + Z_nfcbfd);

   return;
}
