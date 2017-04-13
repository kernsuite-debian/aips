#include <stdlib.h>
#include <string.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

/* this confilcts with some OS's declaration, not others */
extern int gethostname(char *name, int namelen); /* not declared in a
                                                    header */

#if __STDC__
   void zsbind_(int *sock, char *svc, int *svclen, int *iret)
#else
   void zsbind_(sock, svc, svclen, iret)
   int *sock, *svclen, *iret;
   char *svc;
#endif
/*--------------------------------------------------------------------*/
/*! bind a socket to a given service                                  */
/*# Z IO Network                                                      */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1997                                               */
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
/*   Bind a socket to a known service port number on the local host.  */
/*                                                                    */
/*   Inputs:                                                          */
/*                                                                    */
/*   SOCK     I      An integer used to refer to the socket to be     */
/*                   bound (eg. a socket descriptor under Unix or a   */
/*                   socket handle under win95).                      */
/*                   Precondition: SOCK .NE. 0                        */
/*   SVC      C*?    The name of the service.                         */
/*   SVCLEN   I      The number of characters in SVC.                 */
/*                   Precondition: SVCLEN .GT. 0 and                  */
/*                                 SVC(1:SVCLEN) .NE. ' '             */
/*                                                                    */
/*   Outputs:                                                         */
/*                                                                    */
/*   IRET     I      Return code: 0 socket bound                      */
/*                                1 bad argument values               */
/*                                2 system error while obtaining      */
/*                                  information about the host        */
/*                                3 system error while obtaining      */
/*                                  information about the service     */
/*                                4 system error while binding        */
/*                                  socket                            */
/*                                5 other system exceptions           */
/*                                99 indicates that the network       */
/*                                   interface is not implemented     */
/*                                                                    */
/* Usage notes:                                                       */
/*                                                                    */
/* - The caller should interpret IRET and issue any error messages    */
/*                                                                    */
/* Generic Unix version - assumes BSD-style network headers           */
/*--------------------------------------------------------------------*/
{
    char hostname[MAXHOSTNAMELEN];      /* the name of the host */
    struct hostent *host = NULL;        /* the hosts database entry for
                                           the host */
    char *service_name = NULL;          /* null-terminated version of
                                           the service name, trimmed of
                                           all trailing blanks */
    struct servent *service = NULL;     /* the services database entry
                                           for the service */
    struct sockaddr_in service_address; /* the address to be assigned
                                           to the socket */
    int i;

    /* Check preconditions: */

    if (*sock == 0 || *svclen <= 0)
    {
        *iret = 1;
        return;
    }

    /*
     * Make a null-terminated copy of the service name and remove any
     * trailing blanks:
     */

    service_name = (char *) malloc(*svclen + 1);
    if (service_name == NULL) /* out of memory */
    {
        *iret = 5;
        return;
    }
                                           /* Note that service_name
                                              must be freed on exit
                                              from this point on. */
    strncpy(service_name, svc, *svclen);
    service_name[*svclen] = '\0';
    i = *svclen - 1;
    /* Invariant: i >= 0 && service_name[j] == '\0' for all j such that
                  i < j <= *svclen */
    /* Bound: i */
    while (i != 0 && service_name[i] == ' ')
    {
        service_name[i] = '\0';
        /* service_name[j] == '\0' for all j, i - 1 < j <= *svclen */
        i = i - 1;
    }

    /* Trap null service names: */

    if (i == 0)
    {
        *iret = 1;
        free(service_name);
        return;
    }

    /* Find the internet address of the host: */

    if (gethostname(hostname, MAXHOSTNAMELEN) != 0)
    {
        *iret = 2;
        free(service_name);
        return;
    }
    host = gethostbyname(hostname);
    if (host == NULL)
    {
        *iret = 2;
        free(service_name);
        return;
    }

    /* Find the port number for the service: */

    service = getservbyname(service_name, "tcp");
    if (service == NULL)
    {
        *iret = 3;
        free(service_name);
        return;
    }

    free(service_name); /* no longer needed */

    /*
     * Fill in the socket address structure and bind the socket to
     * that address:
     */

    memset((void *) &service_address, 0, sizeof(service_address));
    service_address.sin_family = AF_INET;
    service_address.sin_port   = service->s_port;
    memcpy((void *) &service_address.sin_addr, host->h_addr, host->h_length);
    if (bind(*sock, (struct sockaddr *) &service_address,
             sizeof(service_address)) != 0)
    {
        *iret = 4;
    }
    else
    {
        *iret = 0;
    }
}
