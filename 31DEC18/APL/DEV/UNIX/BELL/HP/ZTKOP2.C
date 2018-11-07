#define Z_tkop2__
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <fcntl.h>
#ifndef __linux__
#include <sgtty.h>
#endif
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <sys/stat.h>
#include <AIPSPROTO.H>
#ifdef __linux__
#include <linux/termios.h>
#endif

#if __STDC__
   void ztkop2_(int *fcb, char pname[MAXPNAME], int *tktype, int *ierr)
#else
   void ztkop2_(fcb, pname, tktype, ierr)
   int *fcb, *tktype, *ierr;
   char pname[MAXPNAME];
#endif
/*--------------------------------------------------------------------*/
/*! read/write from/to a Tektronix device                             */
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
/*  Open a Tektronix device for exclusive use.                        */
/*                                                                    */
/*  Inputs:                                                           */
/*     pname   C*48   Physical file name (e.g., "TEKTK1: ")           */
/*  Output:                                                           */
/*     fcb     I(*)   File control block for opened Tektronix device  */
/*     ierr    I      Error return code: 0 => no error                */
/*                             2 => no such logical device            */
/*                             3 => invalid device name               */
/*                             4 => exclusive use requested but not   */
/*                                  available                         */
/*                             6 => other open error                  */
/*  Berkeley UNIX version                                             */
/*  JDE 89/Mar/23: Jonathan D. Eisenhamer, UCLA, added ability to     */
/*                 talk to a "server" for a separate tektool window.  */
/*  JDE 89/Apr/05: Added ability to use the INET domain to talk to    */
/*                 the tektool service.  The server is tekserver.     */
/*                 The environmental variable TEKTK* should be set to */
/*                 the machine name which has the server.             */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   int llen, xlen, xlnb, jerr, fd;
   register int i;
   struct stat mybuf;
   char lognam[MAXPNAME], pathname[MAXAPATH], *device, *dtemp, end;
   struct hostent *hp;                  /* Host name */
   struct servent *sp;                  /* Internet service info. */
   int s;                               /* Socket descriptor. */
   struct sockaddr_in sock_name;        /* Socket name. */
   struct sockaddr_un client;
   int ishost;                          /* == 1 if it is a host name. */
   int isspec;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *tktype = 0;
   end = '\0' ;
   device = &end;
   if ((dtemp = getenv("TVDEV")) != NULL) {
      if ((device = getenv (dtemp)) == NULL) device = &end;
      }
                                        /* Extract logical device     */
                                        /* name.                      */
   for (i = 0; i < MAXPNAME && pname[i] != ':'; i++)
      lognam[i] = pname[i];
   lognam[i] = '\0';
                                        /* Translate logical device   */
                                        /* name and null terminate.   */
   llen = MAXPNAME;
   xlen = MAXAPATH;
   ztrlo2_ (&llen, lognam, &xlen, pathname, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 2;
      }
   else {
      pathname[xlnb] = '\0';
                                        /* Now, if it's NOT special,  */
                                        /* assume that it is meant to */
                                        /* be an INET domain socket   */
      if (strncmp(device,"sssin",5) == 0) {
         if ((hp = gethostbyname (pathname)) == 0) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 3;
            return;
            }
                                        /* JDE 89/Apr/04:             */
                                        /* Get the port on which the  */
                                        /* the tekserver resides.     */
         if ((sp = getservbyname ("tekserv", "tcp")) == 0) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 3;
            return;
            }
                                        /* Construct the address of   */
                                        /* service.                   */
         memset ((char *)&sock_name, 0, sizeof (sock_name));
         memcpy ((char *)&sock_name.sin_addr, hp->h_addr, hp->h_length);
         sock_name.sin_family = hp->h_addrtype;
         sock_name.sin_port = sp->s_port;
                                        /* Create the communications  */
                                        /* socket.                    */
         if ((s = socket (hp->h_addrtype, SOCK_STREAM, 0)) == -1) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 6;
            return;
            }
                                        /* Connect to the server.     */
                                        /* From here on, everything   */
                                        /* is treated the same.  Only */
                                        /* thing to watch is how to   */
                                        /* close things (ztkcl2)      */
         if (connect (s, (struct sockaddr *)&sock_name,
            sizeof(sock_name)) == -1) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 6;
            }
         else {
             *(fcb + Z_fcbfd) = s;
             *tktype = 1;
             }
         }
                                        /* Test path name existence,  */
                                        /* but DO NOT be fooled by    */
                                        /* files other than a socket  */
                                        /* or a device (char/block)   */
      else {
                                        /* no such file, not special  */
         isspec = 0;
         if ((stat(pathname, &mybuf)) == -1) {
            isspec = 0;
                                        /* Found a socket; special    */
         } else if (S_ISSOCK(mybuf.st_mode)) {
            isspec = 2;
                                        /* Found a FIFO; special      */
         } else if (S_ISFIFO(mybuf.st_mode)) {
            isspec = 2;
                                        /* character special device   */
         } else if (S_ISCHR(mybuf.st_mode)) {
            isspec = 3;
                                        /* block special device       */
         } else if (S_ISBLK(mybuf.st_mode)) {
            isspec = 4;
            }
                                        /* UNIX socket */
         if (isspec== 2) {   /* ????? */
            device = &end;
            if ((dtemp = getenv("TKDEV")) != NULL) {
               if ((device = getenv(dtemp)) == NULL) device = &end;
               }
                                        /* undefined                  */
            if (device == &end) {
               fprintf(stderr, "ZTKOP2: NO SUCH DEVICE = TKDEV\n");
               *ierr = 2;
               return;
               }
            if ((*fcb = (int)socket(AF_UNIX, SOCK_STREAM, 0)) < 0) {
               perror("ZTKOP2 socket (UNIX)");
               *ierr = 1;
               return;
               }
            client.sun_family = AF_UNIX;
            strcpy(client.sun_path, device);
            if (connect((*fcb), (struct sockaddr *) &client,
               (sizeof(client.sun_path)+sizeof(client.sun_family)))
               < 0) {
               perror("ZTKOP2 connect (UNIX)");
               *ierr = 1;
               return;
               }
            *tktype = 1;
            }
                                        /* Blindly change the device  */
                                        /* permissions to make it     */
                                        /* readable/writeable.  Don't */
                                        /* worry about any errors in  */
                                        /* doing so because "open"    */
                                        /* will detect any failure.   */
                                        /* This is done simply to     */
                                        /* allow /dev/ttyp* opens.    */
         else {
            chmod (pathname, 0662);
                                        /* Open the Tektronix device  */
                                        /* read/write.                */
            if ((*(fcb + Z_fcbfd) = open (pathname, 2)) == -1) {
               *(fcb + Z_fcberr) = errno;
               if (errno == EBUSY)
                  *ierr = 4;
               else
                  *ierr = 6;
               }
            else {
               fd = *(fcb + Z_fcbfd);
                                        /* no TIOCEXCL for HP-UX */
               }
            }
         }
      }

   return;
}
