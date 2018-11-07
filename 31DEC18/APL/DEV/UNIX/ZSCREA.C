#include <sys/types.h>
#include <sys/socket.h>

#if __STDC__
   void zscrea_(int *sock, int *iret)
#else
   void zscrea_(sock, iret)
   int *sock, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! create an internet socket                                         */
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
/*   Create an internet stream (TCP) socket.  The resulting socket    */
/*   may be used in a server (see ZSBIND, ZSLISN and ZSACPT) or a     */
/*   client program (see ZSCONN).                                     */
/*                                                                    */
/*   Outputs:                                                         */
/*                                                                    */
/*   SOCK     I      An integer used to refer to the socket just      */
/*                   created (eg. a socket descriptor under Unix or   */
/*                   a socket handle under win95)                     */
/*   IRET     I      Return code: 0 indicates that the socket was     */
/*                                  created                           */
/*                                1 indicates that a system-level     */
/*                                  error occurred                    */
/*                                99 indicates that the network       */
/*                                   interface is not implemented     */
/*                                                                    */
/* Usage notes:                                                       */
/*                                                                    */
/* - SOCK will be non-zero if IRET is zero                            */
/* - Tasks should check for IRET = 99 to test whether network inter-  */
/*   faces are available in AIPS on the host system.                  */
/* - The caller should interpret IRET and issue any error messages    */
/*                                                                    */
/* Generic Unix version - assumes BSD-style network headers           */
/*--------------------------------------------------------------------*/
{
    *sock = socket(AF_INET, SOCK_STREAM, 0);
    if (*sock == -1)
    {
        *iret = 1;
    }
    else
    {
        *iret = 0;
    }
}
