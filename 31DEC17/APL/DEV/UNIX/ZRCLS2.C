#define Z_rcls2__
#include <unistd.h>
#include <errno.h>

#if __STDC__
   void zrcls2_ (int *fcb, int *iret)
#else
   void zrcls2_ (fcb, iret)
   int *fcb, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! close a connection to a server                                    */
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
/*   Close a connection to a server.                                  */
/*   Inputs:                                                          */
/*      FCB      I(*)          AIPS file control block.               */
/*   Outputs:                                                         */
/*      IRET     I             Error return status:                   */
/*                                0 => channel closed                 */
/*                                1 => failed to close channel        */
/*   BSD Unix version (uses socket interface).                        */
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd;                  /* offset of file descriptor  */
                                        /* in file control block      */
   extern int Z_fcberr;                 /* offset of error code in    */
                                        /* file control block         */
/*--------------------------------------------------------------------*/
   if ( close(*(fcb + Z_fcbfd)) == -1 ) {
      *iret = 1;
      *(fcb + Z_fcberr) = errno;
      }
   else {
      *iret = 0;
      *(fcb + Z_fcberr) = 0;
      }
   return;
}

