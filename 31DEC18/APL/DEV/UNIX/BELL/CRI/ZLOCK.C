#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/file.h>

#define MAXPNAME 48                     /* Maximum logical device     */
                                        /* plus file name length.     */
#define MAXPATH 128                     /* Maximum path name length.  */

zlock_(fd, pname, excl, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! apply a shared or exclusive lock to a disk file                   */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1998                                               */
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
/*--------------------------------------------------------------------*/
/*  Open (if necessary) and apply either a shared or exclusive lock   */
/*  a file.  If an error occurs, files are NOT closed.                */
/*  Inputs:                                                           */
/*     pname    C*48   Physical file name                             */
/*     excl     I      Lock type indicator: 0 => shared               */
/*                                          1 => exclusive            */
/*  In/out:                                                           */
/*     fd       I      File descriptor (if -1, open the file as well  */
/*                        as apply the requested lock; return the     */
/*                        file descriptor)                            */
/*  Output:                                                           */
/*     syserr   I      System error code                              */
/*     ierr     I      Error return code: 0 => no error               */
/*                        1 => device not found (fd = -1)             */
/*                        2 => file not found (fd = -1)               */
/*                        3 => open error (fd = -1)                   */
/*                        4 => exclusive use denied                   */
/*                        6 => other lock error                       */
/*  UNICOS version (Berkeley UNIX "flock" supported).                 */
/*--------------------------------------------------------------------*/
int *fd, *excl, *syserr, *ierr;
char pname[MAXPNAME];
/*--------------------------------------------------------------------*/
{
   extern int errno;
   int llen, xlen, xlnb, jerr;
   char pathname[MAXPATH];
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *syserr = 0;
                                        /* If *fd = -1, we need to    */
                                        /* open the file first.       */
   if (*fd == -1) {
                                        /* Translate path name        */
      llen = MAXPNAME;
      xlen = MAXPATH;
      zpath_ (&llen, pname, &xlen, pathname, &xlnb, &jerr);
      if (jerr != 0) {
         *ierr = 1;
         }
      else {
         pathname[xlnb] = '\0';
                                        /* Open the disk file         */
                                        /* read/write.                */
         if ((*fd = open (pathname, 2)) == -1) {
            *syserr = errno;
                                        /* File doesn't exist.        */
            if (errno == ENOENT)
               *ierr = 2;
                                        /* Some other open error.     */
            else
               *ierr = 3;
            perror ("ZLOCK");
            printf ("ZLOCK: Pathname %s\n",pathname);
            goto exit;
            }
         }
      }
                                        /* Apply non-blocking lock.   */
                                        /* Exclusive lock.            */
   if (*excl == 1) {
      if (flock (*fd, LOCK_EX | LOCK_NB) != 0) {
         *syserr = errno;
                                        /* Not available.             */
         if (errno == EWOULDBLOCK) {
            *ierr = 4;
            }
                                        /* Some other flock error.    */
         else {
            perror ("ZLOCK");
            *ierr = 6;
            }
         }
      }
                                        /* Shared lock.               */
   else {
      if (flock (*fd, LOCK_SH | LOCK_NB) != 0) {
         *syserr = errno;
                                        /* Not available.             */
         if (errno == EWOULDBLOCK) {
            *ierr = 4;
            }
                                        /* Some other flock error.    */
         else {
            perror ("ZLOCK");
            *ierr = 6;
            }
         }
      }

exit:
   return;
}
