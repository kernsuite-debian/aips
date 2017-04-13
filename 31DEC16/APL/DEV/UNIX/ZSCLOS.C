#include <unistd.h>

#if __STDC__
   void zsclos_(int *sock, int *iret)
#else
   void zsclos_(sock, iret)
   int *sock, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! close an internet socket                                         */
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
/*   Close the local end of an internet connection.                   */
/*                                                                    */
/*   Inputs:                                                          */
/*                                                                    */
/*   SOCK     I      An integer designating the socket to be closed,  */
/*                   as returned by ZSCREA.                           */
/*                   Precondition: SOCK .NE. 0                        */
/*                                                                    */
/*   Outputs:                                                         */
/*                                                                    */
/*   IRET     I      Return code: 0 indicates that the socket was     */
/*                                  created                           */
/*                                1 indicates that ZSCLOS was called  */
/*                                  with bad arguments                */
/*                                2 indicates that a system-level     */
/*                                  error was detected                */
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
    /* Trap bad socket values: */

    if (*sock == 0)
    {
        *iret = 1;
    }
    else
    {
        if (close(*sock) != 0)
        {
            *iret = 2;
        }
        else
        {
            *iret = 0;
        }
    }
}
