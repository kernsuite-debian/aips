#include <unistd.h>
#include <sys/types.h>
#include <sys/param.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>
#include <netdb.h>

#if __STDC__
   void zsacpt_(int *sock, int *clilen, int *newsoc, char *client,
      int *iret)
#else
   void zsacpt_(sock, clilen, newsoc, client, iret)
   int *sock, *clilen, *newsoc, *iret;
   char *client;
#endif
/*--------------------------------------------------------------------*/
/*! accept a connection from a client                                 */
/*# Z IO Network                                                      */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1997, 2008                                         */
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
/*   Accept a connection from a client.  Should only be called for    */
/*   sockets which have been set into "listen" mode using ZSLISN.     */
/*                                                                    */
/*   Inputs:                                                          */
/*                                                                    */
/*   SOCK     I      An integer used to refer to the socket to be     */
/*                   be used, as returned from ZSCREA.                */
/*                   Precondition: SOCK .NE. 0                        */
/*   CLILEN   I      The length of the buffer used to receive the     */
/*                   client name (CLIENT below).                      */
/*                   Precondition: CLILEN > 0                         */
/*                                                                    */
/*   Outputs:                                                         */
/*                                                                    */
/*   NEWSOC   I      The ID number of a new socket.  Use this socket, */
/*                   not SOCK, to read and write to the client.       */
/*   CLIENT   C*?    The name of the client.                          */
/*   IRET     I      Return code: 0 connection accepted               */
/*                                1 a bad argument value was detected */
/*                                2 a system error occurred           */
/*                                99 indicates that the network       */
/*                                   interface is not implemented     */
/*                                                                    */
/* Usage notes:                                                       */
/*                                                                    */
/* - The caller should interpret IRET and issue any error messages    */
/* - NEWSOC will be non-zero if no errors are detected.               */
/* - Program execution is suspended until a connection is made.       */
/* - The client name will be truncated if it does not fit in CLIENT.  */
/*                                                                    */
/* Generic Unix version - assumes BSD-style network headers           */
/*--------------------------------------------------------------------*/
{
    struct sockaddr_in client_address; /* the socket address of the client */
    int client_address_length;         /* the size of the client_address
                                          structure */
    struct hostent *client_host;       /* the hosts database entry for the
                                          client machine */
    char *client_name = NULL;          /* the name of the client machine */
    int i;

    /* Check preconditions: */

    if (*sock == 0 || *clilen <= 0)
    {
        *iret = 1;
        return;
    }

    client_address_length = sizeof(client_address);
    *newsoc = accept(*sock, (struct sockaddr *) &client_address,
                     &client_address_length);
    if (*newsoc == -1)
    {
        *iret = 2;
        return;
    }
    else
    {
        *iret = 0;
    }

    /*
     * Look the client machine's name up based on its address.  If it is
     * not in the hosts database then just used the "dotted" form of its
     * internet address:
     */

    client_host = gethostbyaddr((const char *) &client_address.sin_addr,
                                4, AF_INET);
    if (client_host == NULL)
    {
        client_name = inet_ntoa(client_address.sin_addr);
    }
    else
    {
        client_name = client_host->h_name;
    }

    /*
     * Copy the client's name to the FORTRAN output variable, adding
     * trailing blanks if necessary:
     */

    i = 0;
    /* Invariant: client[j] = client_name[j], for j such that 0 <= j < i */
    /* Bound: *clilen - i */
    while (i < *clilen && client_name[i] != '\0')
    {
        client[i] = client_name[i];
        i = i + 1;
    }
    /* Invariant: client[j] = ' ', for j such that
                                   strlen(client_name) <= j < *clilen */
    /* Bound: *clilen - i */
    while (i < *clilen)
    {
        client[i] = ' ';
        i = i + 1;
    }
}
