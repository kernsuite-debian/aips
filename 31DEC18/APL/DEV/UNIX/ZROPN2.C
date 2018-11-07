#define Z_ropn2__
#include <stdlib.h>
#include <sys/types.h>
#include <errno.h>
#include <string.h>
#include <netdb.h>
#include <stdio.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#ifndef INADDR_NONE
#define INADDR_NONE 0xffffffff
#endif

#if __STDC__
   void zropn2_ (int *fcb, int *srvlen, char *srvnam, int *hstlen,
      char *hstnam, int *iret)
#else
   void zropn2_ (fcb, srvlen, srvnam, hstlen, hstnam, iret)
   int *fcb, *srvlen, *hstlen, *iret;
   char *srvnam, *hstnam;
#endif
/*--------------------------------------------------------------------*/
/*! request a connection to a server -- BSD Unix version              */
/*# IO-remote Z2                                                      */
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
/*   Request a connection to a server.                                */
/*                                                                    */
/*   Inputs:                                                          */
/*      FCB         I(*)          File control block.                 */
/*      SRVLEN      I             Length of server name including     */
/*                                trailing blanks.                    */
/*      SRVNAM      C*(*)         Server name.                        */
/*      HSTLEN      I             Length of host name including       */
/*                                trailing blanks.                    */
/*      HSTNAM      C*(*)         Host name.                          */
/*                                                                    */
/*   Output:                                                          */
/*      IRET        I             Error return status:                */
/*                                   0 => channel opened successfully */
/*                                   1 => request failed              */
/*                                                                    */
/*   BSD Unix version --- uses INET domain stream sockets.            */
/*      The server name may either be a named service or a port       */
/*      number.                                                       */
/*      The host name may be a name or a decimal Internet address.    */
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd;                  /* offset of file descriptor  */
                                        /* file control block         */
   extern int Z_fcberr;                 /* offset of system error code*/
                                        /* in file control block      */
   char *service_name;                  /* null-terminated service    */
                                        /* name                       */
   char *host_name;                     /* null-terminated host name  */

   struct servent *service;             /* service database entry     */
   struct hostent *host;                /* host database entry        */

   unsigned long inaddr;                /* Internet address of host   */

   struct sockaddr_in address;          /* INET address of server     */

   int sd;                              /* socket descriptor          */

   char *ptr1, *ptr2;
/*--------------------------------------------------------------------*/
   *(fcb + Z_fcberr) = 0; *iret = 0;
                                       /* Initialize address           */
                                       /* structure:                   */
   memset ((char *) &address, 0, sizeof(address));
   address.sin_family = AF_INET;

                                        /* Copy service name into      */
                                        /* local storage:              */
   if ( (service_name = (char *) malloc (*srvlen)) == NULL ) {
      *iret = 1;
      *(fcb + Z_fcberr) = errno;
      }
   else {
      ptr1 = srvnam;
      ptr2 = service_name;
      while ( (ptr1 != srvnam + *srvlen) && (*ptr1 != ' ') ) {
         *ptr2++ = *ptr1++;
         }
      *ptr2 = '\0';
      }

                                       /* Copy host name into local    */
                                       /* storage:                     */
   if ( *iret == 0 ) {
      if ( (host_name = (char *) malloc (*hstlen)) == NULL ) {
         *iret = 1;
         *(fcb + Z_fcberr) = errno;
         }
      else {
         ptr1 = hstnam;
         ptr2 = host_name;
         while ( (ptr2 != hstnam + *hstlen) && (*ptr1 != ' ') ) {
            *ptr2++ = *ptr1++;
            }
         *ptr2 = '\0';
         }
      }

   if ( *iret == 0 ) {
                                       /* Translate service name to a  */
                                       /* port number:                 */
      if ( (service = getservbyname (service_name, "tcp")) != NULL ) {
                                       /* Look-up succeeded.           */
         address.sin_port = service->s_port;
         }
      else {
                                       /* Look-up failed.              */
                                       /* Fill in port number field of */
                                       /* address assuming that        */
                                       /* service_name is a port       */
                                       /* number:                      */
         address.sin_port = htons (atoi (service_name));
         }
      }

   if ( *iret == 0 ) {
                                       /* Translate the host name to   */
                                       /* an Internet address:         */
      if ( (inaddr = inet_addr(host_name)) != INADDR_NONE ) {
                                       /* Host name was a dotted       */
                                       /* decimal number.              */
         memcpy ((char *) &address.sin_addr, (char *) &inaddr,
               sizeof(inaddr));
         }
      else {
                                       /* Look-up name in network      */
                                       /* database:                    */
         if ( (host = gethostbyname(host_name)) == NULL ) {
            *iret = 1; *(fcb+Z_fcberr) = errno;
            }
         else {
            memcpy ((char *) &address.sin_addr, host->h_addr,
                  host->h_length);
            }
         }
      }

   if ( *iret == 0 ) {
                                       /* Create TCP socket:           */
      if ( (sd = socket(AF_INET, SOCK_STREAM, 0)) == -1 ) {
         *iret = 1;
         *(fcb + Z_fcberr) = errno;
         }
      }

   if ( *iret == 0 ) {
                                      /* Request connection:           */
      if ( connect(sd, (struct sockaddr *) &address, sizeof(address)) ==
           -1 ) {
         *iret = 1;
         *(fcb + Z_fcberr) = errno;
         }
      else {
         *(fcb + Z_fcbfd) = sd;
         *(fcb + Z_fcberr) = 0;
         }
      }

                                       /* Release dynamically          */
                                       /* allocated memory:            */
   if ( service_name != NULL ) {
      (void) free(service_name);
      }
   if ( host_name != NULL ) {
      (void) free(host_name);
      }

   return;
}
