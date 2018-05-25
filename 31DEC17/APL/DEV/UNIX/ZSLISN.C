#include <sys/types.h>
#include <sys/socket.h>


#if __STDC__
   void zslisn_(int *sock, int *qlen, int *iret)
#else
   void zslisn_(sock, qlen, iret)
   int *sock, *qlen, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! Place a socket into "listen" mode                                 */
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
/*   Set a socket into "listen" mode so that client programs may      */
/*   connect to it (use ZSACPT to accept incoming connections after   */
/*   setting the socket mode).                                        */
/*                                                                    */
/*   Inputs:                                                          */
/*                                                                    */
/*   SOCK     I      An integer designating the socket to use, as     */
/*                   returned by ZSCREA.                              */
/*                   Precondition: SOCK .NE. 0                        */
/*   QLEN     I      The number of pending connections to be queued   */
/*                   for servicing.                                   */
/*                   Precondition: QLEN .GE. 0                        */
/*   Outputs:                                                         */
/*                                                                    */
/*   IRET     I      Return code: 0 indicates that the socket is is   */
/*                                  listen mode                       */
/*                                1 indicates an invalid argument     */
/*                                  value                             */
/*                                2 indicates that a system error     */
/*                                  occurred                          */
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
    /* Check preconditions: */

    if (*sock == 0 || *qlen < 0)
    {
        *iret = 1;
        return;
    }

    if (listen(*sock, *qlen) == 0)
    {
        *iret = 0;
    }
    else
    {
        *iret = 2;
    }
}
