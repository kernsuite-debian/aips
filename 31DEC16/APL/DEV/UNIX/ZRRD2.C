#define Z_rrd2__
#include <unistd.h>
#include <sys/types.h>
#include <errno.h>
#include <netinet/in.h>

#if __STDC__
   void zrrd2_ (int *fcb, int *nbytes, int *buff, int *iret)
#else
   void zrrd2_ (fcb, nbytes, buff, iret)
   int *fcb, *nbytes, *buff, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! read a message from a server -- BSD Unix version                  */
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
/*   Read a message from the server.                                  */
/*                                                                    */
/*   Inputs:                                                          */
/*      FCB        I(*)        AIPS file control block                */
/*                                                                    */
/*   Input/Output:                                                    */
/*      NBYTES     I           On input the maximum number of bytes   */
/*                             to receive; on output the number of    */
/*                             bytes actually read.                   */
/*                                                                    */
/*   Outputs:                                                         */
/*      BUFF       I(*)        Message buffer                         */
/*      IRET       I           Error return code:                     */
/*                                0 => message read successfully;     */
/*                                1 => message longer than NBYTES;    */
/*                                2 => system error;                  */
/*                                3 => partner closed connection.     */
/*                                                                    */
/*   BSD Unix version.  Reads from a socket stream.  The message is   */
/*   preceded by a 4-byte integer (in network byte order) giving its  */
/*   length in bytes.                                                 */
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr;

   int chunk;                          /* number of bytes read in     */
                                       /* most recent read operation  */
   int  msglen;                        /* message length (network     */
                                       /* byte order)                 */
   int bytes_left;                     /* number of bytes still to be */
                                       /* read in header or message   */
   char *ptr;
/*--------------------------------------------------------------------*/
   *iret = 0;

                                       /* Read the length of the      */
                                       /* message:                    */
   bytes_left = 4;
   ptr = (char *) &msglen;
   while ( *iret == 0 && bytes_left != 0 ) {
      chunk = read (*(fcb+Z_fcbfd), ptr, bytes_left);
      if ( chunk == -1 ) {
         *iret = 2;
         *(fcb+Z_fcberr) = errno;
         }
      else if ( chunk == 0 ) {
         *iret = 3;
         }
      else {
         bytes_left -= chunk;
         ptr += chunk;
         }
      }
   *(fcb+Z_fcbreq) = ntohl(msglen);

   if ( *iret == 0 ) {
                                       /* Check message length        */
                                       /* against maximum:            */
      if ( *(fcb+Z_fcbreq) > *nbytes ) {
         *iret = 1;
         }
      }

   if ( *iret == 0 ) {
                                       /* Read the message:           */
      bytes_left = *(fcb+Z_fcbreq);
      ptr = (char *) buff;
      while ( *iret == 0 && bytes_left != 0 ) {
         chunk = read (*(fcb+Z_fcbfd), ptr, bytes_left);
         if ( chunk == 1 ) {
            *iret = 2;
            *(fcb+Z_fcberr) = errno;
            }
         else if ( chunk == 0 ) {
            *iret = 3;
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
