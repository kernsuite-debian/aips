#define Z_rxdw2__
#include <sys/types.h>
#include <sys/socket.h>
#include <errno.h>

#if __STDC__
   void zrxdw2_ (int *fcb, int *code, int *iret)
#else
   void zrxdw2_ (fcb, code, iret)
   int  *fcb, *code, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! write one byte of expedited data to a remote process -- BSD       */
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
/*   Write one byte of expedited data to a remote process.            */
/*   Inputs:                                                          */
/*      FCB       I(*)    AIPS file control block                     */
/*      CODE      I       Byte to send                                */
/*   Outputs:                                                         */
/*      IRET      I       Error return code                           */
/*                           0 => byte sent                           */
/*                           1 => write failed                        */
/*                           2 => partner closed connection           */
/*   BSD version: writes out-of-band data to a stream socket.         */
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr;
   char byte;
   int status;
/*--------------------------------------------------------------------*/
   *iret = 0;
   *(fcb+Z_fcbreq) = 1;
   byte = (char) *code;

   if ( (status = send (*(fcb+Z_fcbfd), &byte, 1, MSG_OOB)) == -1 ) {
      *iret = 1;
      *(fcb+Z_fcberr) = errno;
      }
   else if ( status == 0 ) {
      *iret = 2;
      }
   else {
      *(fcb+Z_fcbxfr) = 1;
      }

   return;
}
