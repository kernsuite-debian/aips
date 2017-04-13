#include <unistd.h>

#if __STDC__
   void zsread_(int *sock, int *dlen, int *data, int *iret)
#else
   void zsread_(sock, dlen, data, iret)
   int *sock, *dlen, *data, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! read data from a network connection.                              */
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
/*   Read data from the network.                                      */
/*                                                                    */
/*   Inputs:                                                          */
/*                                                                    */
/*   SOCK     I      An integer designating the socket to be read     */
/*                   from as returned by ZSCREA.                      */
/*                   Precondition: SOCK .NE. 0                        */
/*   DLEN     I      The number of bytes of data to write             */
/*                   Precondition: DLEN .GE. 0                        */
/*                                                                    */
/*   Outputs:                                                         */
/*                                                                    */
/*   DATA     I(*)   Data buffer                                      */
/*   IRET     I      Return code: 0 indicates that the data was read  */
/*                                1 indicates that ZSREAD was called  */
/*                                  with bad arguments                */
/*                                2 indicates that a system-level     */
/*                                  error was detected                */
/*                                -1 indicates that the remote end of */
/*                                   the connection was closed        */
/*                                99 indicates that the network       */
/*                                   interface is not implemented     */
/*                                                                    */
/* Usage notes:                                                       */
/*                                                                    */
/* - If IRET is -1 the local end of the connection should be closed;  */
/*   any amount of data, short of DLEN bytes may have been tranferred */
/*   from the peer process.                                           */
/* - The calling process will be blocked until either all of the data */
/*   is read, the remote end of the connection is closed or an error  */
/*    occurs                                                          */
/* - The caller should interpret IRET and issue any error messages    */
/*                                                                    */
/* Generic Unix version - assumes BSD-style network headers           */
/*--------------------------------------------------------------------*/
{
    char *buffer = (char *) data;       /* pointer to next byte of data
                                           to transfer */
    int  bytes_left = *dlen;            /* number of bytes left to
                                           write */
    int  bytes_read = 0;                /* number of bytes read in a
                                           single chunk */

    /* Trap bad input values: */

    if (*sock == 0 || *dlen < 0)
    {
        *iret = 1;
    }
    else
    {
        /* loop until all data has been read */

        while (bytes_left > 0)
        {
            bytes_read = read(*sock, buffer, bytes_left);
            if (bytes_read == -1) /* exception */
            {
                *iret = 2;
                return;
            }
            else if (bytes_read == 0) /* connection closed */
            {
                *iret = -1;
                return;
            }
            else
            {
                buffer = buffer + bytes_read;
                bytes_left = bytes_left - bytes_read;
            }
        }
        *iret = 0;
    }
}
