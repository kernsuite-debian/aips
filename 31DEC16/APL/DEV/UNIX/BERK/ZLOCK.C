                                        /* force use of fcntl for NFS */
                                        /* If you are having problems */
					/* with lock daemon version */
					/* mismatches, you may want to */
					/* remove the next line and live */
					/* with the inferior network- */
					/* blind flock() routine.  It */
					/* can let two processes on */
					/* different hosts lock the same */
					/* file, however so be WARNED! */
#define _POSIX_SOURCE
#ifdef _POSIX_SOURCE
#include <sys/types.h>
#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#else
#include <stdio.h>
#include <errno.h>
#include <sys/file.h>
#endif /* _POSIX_SOURCE */

#define MAXPNAME 48                     /* Maximum logical device     */
                                        /* plus file name length.     */
#ifdef _POSIX_SOURCE
#define MAXPATH _POSIX_PATH_MAX
#else
#define MAXPATH 128                     /* Maximum path name length.  */
#endif /* _POSIX_SOURCE */

#ifndef _POSIX_SOURCE
#ifndef O_RDWR
#define O_RDWR 2
#endif
#endif

zlock_(fd, pname, excl, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! apply a shared or exclusive lock to a disk file                   */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1998, 2004                                   */
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
/*  4.2bsd UNIX version/POSIX version -- selects POSIX interface if   */
/*  _POSIX_SOURCE is defined.                                         */
/*--------------------------------------------------------------------*/
int *fd, *excl, *syserr, *ierr;
char pname[MAXPNAME];
/*--------------------------------------------------------------------*/
{
   int llen, xlen, xlnb, jerr;
   char pathname[MAXPATH];
#ifdef _POSIX_SOURCE
   struct flock lock;
#endif
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
         if ((*fd = open (pathname, O_RDWR)) == -1) {
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

#ifdef _POSIX_SOURCE
                                        /* Apply non-blocking lock.   */
   lock.l_whence = SEEK_SET; lock.l_start = 0; lock.l_len = 0;
                                        /* Exclusive lock.            */
   if ( *excl == 1) {
      lock.l_type = F_WRLCK;
      }
   else {                               /* Shared lock                */
      lock.l_type = F_RDLCK;
      }

   if ( fcntl (*fd, F_SETLK, &lock) == -1 ) {
      *syserr = errno;
      if ( errno == EACCES || errno == EAGAIN ) {
         *ierr = 4;
         }
                                        /* Some other error           */
                                        /* mask on read-only systems  */
      else if (*excl != -1) {
         perror ("ZLOCK");
         *ierr = 6;
         }
      }
#else /* _POSIX_SOURCE */

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
                                        /* Some other error           */
                                        /* mask on read-only systems  */
         else if (*excl != -1) {
            perror ("ZLOCK");
            *ierr = 6;
            }
         }
      }
#endif /* _POSIX_SOURCE */

exit:
   return;
}
