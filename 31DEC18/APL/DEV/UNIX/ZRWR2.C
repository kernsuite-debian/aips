#define Z_rwr2__
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <netinet/in.h>

#if __STDC__
   void zrwr2_ (int *fcb, int *nbytes, int *buff, int *iret)
#else
   void zrwr2_ (fcb, nbytes, buff, iret)
   int *fcb, *nbytes, *buff, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! write a message to a server                                       */
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
/*   Write a message to a server.                                     */
/*   Inputs:                                                          */
/*      FCB        I(*)        AIPS file control block                */
/*      NBYTES     I           Number of bytes to transfer            */
/*      BUFF       I(*)        Message buffer                         */
/*   Outputs:                                                         */
/*      IRET       I           Error return code:                     */
/*                                0 => message written                */
/*                                1 => write failed                   */
/*                                2 => partner closed connection.     */
/*   BSD Unix version.  Writes to a stream socket.  The message is    */
/*   preceded by a 4-byte integer giving its length in network byte   */
/*   order.                                                           */
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr;
   int msglen;                          /* message length (network    */
                                        /* byte order                 */
   int chunk;                           /* number of bytes in last    */
                                        /* transfer                   */
   int bytes_left;                      /* number of bytes still to   */
                                        /* be transferred             */
   char *ptr;
/*--------------------------------------------------------------------*/
   *iret = 0;
   *(fcb+Z_fcbreq) = *nbytes;
   msglen = htonl(*nbytes);

                                       /* Transfer header:            */
   bytes_left = 4;
   ptr = (char *) &msglen;
   while ( *iret == 0 && bytes_left != 0 ) {
      chunk = write (*(fcb+Z_fcbfd), ptr, bytes_left);
      if ( chunk == -1 ) {
         *iret = 1;
         *(fcb+Z_fcberr) = errno;
         }
      else if ( chunk == 0 ) {
         *iret = 2;
         }
      else {
         bytes_left -= chunk;
         ptr += chunk;
         }
      }

   if ( *iret == 0 ) {
                                       /* Transfer message:           */
      bytes_left = *nbytes;
      ptr = (char *) buff;
      while ( *iret == 0 && bytes_left != 0 ) {
         chunk = write (*(fcb+Z_fcbfd), ptr, bytes_left);
         if ( chunk == -1 ) {
            *iret = 1;
            *(fcb+Z_fcberr) = errno;
            }
         else if ( chunk == 0 ) {
            *iret = 2;
            }
         else {
            bytes_left -= chunk;
            ptr += chunk;
            }
         }
      }

   if ( *iret == 0 ) {
      *(fcb+Z_fcbxfr) = *(fcb+Z_fcbreq);
      }

   return;
}
