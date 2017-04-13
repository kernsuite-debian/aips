#define Z_crea2__
#include <unistd.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdio.h>
#include <fcntl.h>
#include <string.h>
#include <errno.h>
#include <stdlib.h>
#include <AIPSPROTO.H>

#define BUFSIZE 1048576                 /* Buffer size for space      */
                                        /* reservation process.       */
#define PMODE (S_IRUSR | S_IWUSR | S_IRGRP | S_IWGRP | S_IROTH | S_IWOTH)

#if __STDC__
   void zcrea2_(char pname[MAXPNAME], int *ablocks, int *qcreat,
      int *asize, int *syserr, int *ierr)
#else
     void zcrea2_(pname, ablocks, qcreat, asize, syserr, ierr)
   char pname[MAXPNAME];
int *ablocks, *qcreat, *asize, *syserr, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! create the specified disk file                                    */
/*# Z2 IO-basic                                                       */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997, 2000, 2002, 2010-2012                   */
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
/*  Create a disk file of the specified name and size with read/write */
/*  permission.                                                       */
/*  Inputs:                                                           */
/*     pname    C*48   Physical file name (MAXPNAME characters)       */
/*     ablocks  I      Requested size of file in AIPS-blocks          */
/*                        (256 local integers)                        */
/*  Output:                                                           */
/*     asize    I      Actual size of file created in AIPS-blocks     */
/*     syserr   I      System error code                              */
/*     ierr     I      Error return code: 0 => no error               */
/*                        1 => file already exists                    */
/*                        2 => volume not found                       */
/*                        3 => insufficient space                     */
/*                        4 => other                                  */
/*  Bell UNIX version (attempts atomic file creations).               */
/*--------------------------------------------------------------------*/
{
                                        /* # bytes per AIPS-byte      */
   extern int Z_nbpab;
   int llen, xlen, xlnb, jerr, fd, tries, lockfd, tdelay,
      iexcl = 1, n6 = 6, n8 = 8;
   int chunks, rest, xfer, start;
   off_t bytes;
   register int i, j, k;
   float delay = 2.0;
   char lognam[MAXPNAME], xlated[MAXAPATH], filename[MAXPNAME],
      lname[MAXPNAME], lockname[MAXAPATH], pathname[MAXAPATH], msgbuf[80];
   char *buff = NULL;
   struct stat statbuf;
/*--------------------------------------------------------------------*/
   *asize = 0;
   *syserr = 0;
   *ierr = 0;
                                        /* Extract logical device.    */
   for (i = 0; i < MAXPNAME && pname[i] != ':'; i++)
      lognam[i] = pname[i];
   lognam[i] = '\0';

                                        /* Extract filename.          */
   j = 0;
   for (i = i+1; i < MAXPNAME && pname[i] != ' '; i++) {
      filename[j] = pname[i];
      j++;
      }
   filename[j] = '\0';
                                        /* Convert number of AIPS-    */
                                        /* blocks requested to bytes  */
   bytes = *ablocks;
   bytes = bytes * Z_nbpab;
   bytes = bytes * 512;
                                        /* Translate logical device   */
                                        /* name and null terminate.   */
   llen = MAXPNAME;
   xlen = MAXAPATH;
   ztrlo2_ (&llen, lognam, &xlen, xlated, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 2;
      sprintf (msgbuf, "ZCREA2: TROUBLE TRANSLATING LOGICAL = %s",
         lognam);
      zmsgwr_ (msgbuf, &n6);
      }
   else if (bytes <= 0) {
      *ierr = 3;
      sprintf (msgbuf, "ZCREA2: BYTES OVERFLOW, BLOCKS = %d", *ablocks);
      zmsgwr_ (msgbuf, &n6);
      }
   else {
      xlated[xlnb] = '\0';
                                        /* Build full path name of    */
                                        /* the file to be created.    */
      sprintf (pathname, "%s/%s", xlated, filename);
                                         /* "Stat" full path name.    */
      if (stat (pathname, &statbuf) == 0) {
                                        /* File already exists. Get   */
                                        /* its actual size in AIPS-   */
                                        /* bytes.                     */
         *ierr = 1;
         *asize = statbuf.st_size / Z_nbpab / 512;
         }
      else {
                                        /* Build physical file name   */
                                        /* of space lock file.        */
         sprintf (lname, "%s:SPACE", lognam);
                                        /* Build full path name to    */
                                        /* space lock file.           */
         sprintf (lockname, "%s/SPACE", xlated);
                                        /* Open space lock file.      */
         if ((lockfd = open (lockname, 2)) == -1) {
            *syserr = errno;
                                        /* If space lock file doesn't */
                                        /* exist, create it.          */
            if (errno == ENOENT) {
               if ((lockfd =
                  open (lockname, O_CREAT | O_EXCL | O_WRONLY, PMODE))
                  == -1) {
                  *ierr = 4;
                  sprintf (msgbuf,
                  "ZCREA2: ERROR CREATING SPACE LOCK FILE = %s", lname);
                  zmsgwr_ (msgbuf, &n8);
                  }
               }
                                        /* Some other open error.     */
            else {
               *ierr = 4;
               sprintf (msgbuf,
                  "ZCREA2: ERROR OPENING SPACE LOCK FILE = %s", lname);
               zmsgwr_ (msgbuf, &n8);
               }
            }
         }
                                        /* If no error so far,        */
                                        /* continue.                  */
      if (*ierr == 0) {
                                        /* Apply non-blocking lock to */
                                        /* space lock file.  This is  */
                                        /* an attempt to force AIPS   */
                                        /* file creations to be       */
                                        /* atomic.  If it fails, we   */
                                        /* try to create the file     */
                                        /* anyway.                    */
         tries = 0;
         tdelay = 0;
lock:
         zlock_ (&lockfd, lname, &iexcl, syserr, &jerr);
         tries = tries + 1;
         if (jerr == 4) {
                                        /* Space lock file is busy.   */
                                        /* Wait and try again up to   */
                                        /* 30 times at 2 second       */
                                        /* intervals.                 */
            if (tries <= 30) {
               zdelay_ (&delay, &jerr);
               tdelay = tdelay + delay;
               goto lock;
               }
            tries = tries - 1;
            sprintf (msgbuf, "ZCREA2: SPACE LOCK FILE = %s", lname);
            zmsgwr_ (msgbuf, &n8);
            sprintf (msgbuf,
               "ZCREA2: STILL BUSY AFTER %d TRIES IN %d SECONDS",
               tries, tdelay);
            zmsgwr_ (msgbuf, &n8);
            sprintf (msgbuf,
               "ZCREA2: A STOPPED PROCESS MAY HAVE IT MONOPOLIZED");
            zmsgwr_ (msgbuf, &n8);
            sprintf (msgbuf, "ZCREA2: NOTIFY THE AIPS SYSTEM MANAGER");
            zmsgwr_ (msgbuf, &n8);
            sprintf (msgbuf,
               "ZCREA2: PERFORMING NON-ATOMIC FILE CREATION INSTEAD");
            zmsgwr_ (msgbuf, &n8);
            }
                                        /* Some other error locking   */
                                        /* space lock file.           */
         else if (jerr != 0) {
            *ierr = 4;
            sprintf (msgbuf,
               "ZCREA2: ERROR LOCKING SPACE LOCK FILE = %s", lname);
            zmsgwr_ (msgbuf, &n8);
            sprintf (msgbuf,
               "ZCREA2: PERFORMING NON-ATOMIC FILE CREATION INSTEAD");
            zmsgwr_ (msgbuf, &n8);
            }
                                        /* Build full path name of    */
                                        /* the file to be created.    */
         sprintf (pathname, "%s/%s", xlated, filename);
                                        /* Create the file.           */
         if ((fd = open
            (pathname, O_CREAT | O_EXCL | O_WRONLY, PMODE)) == -1) {
                                        /* File already exists.       */
            if (errno == EEXIST) {
               *ierr = 1;
               }
            else {
               *syserr = errno;
               *ierr = 4;
               sprintf (msgbuf, "ZCREA2: CREATE ERROR = %d", errno);
               zmsgwr_ (msgbuf, &n6);
               }
            }
                                        /* Apply non-blocking lock.   */
         else {
            zlock_ (&fd, pname, &iexcl, syserr, &jerr);
                                        /* Allocate memory for a      */
                                        /* temporary I/O buffer.      */
            if ((buff = (char *) malloc (BUFSIZE)) == NULL) {
               *syserr = errno;
               *ierr = 4;
               sprintf (msgbuf, "ZCREA2: MEMORY ALLOCATION ERROR = %d", errno);
               zmsgwr_ (msgbuf, &n6);
               }
            else {
               memset (buff, 0, BUFSIZE);
                                        /* Convert number of AIPS-    */
                                        /* blocks requested to bytes  */
               bytes = *ablocks;
               bytes = bytes * Z_nbpab * 512;
                                        /* Calculate the number of    */
                                        /* BUFSIZE byte chunks this   */
                                        /* represents plus remainder  */
                                        /* (for the sake of speed,    */
                                        /* disk space is reserved by  */
                                        /* writing the file in        */
                                        /* BUFSIZE chunks, then the   */
                                        /* remainder).                */
               chunks = bytes / BUFSIZE;
               rest = bytes % BUFSIZE;
                                        /* Reserve the disk space by  */
                                        /* writing the entire file.   */
                                        /* We can't just write the    */
                                        /* last byte since this would */
                                        /* leave a "hole".  That is,  */
                                        /* "stat" would return the    */
                                        /* desired file size, but     */
                                        /* only one block would       */
                                        /* actually be allocated.     */
                                        /* UNIX really should have a  */
                                        /* function for this (just    */
                                        /* something that would mark  */
                                        /* the blocks as in use).     */
               bytes = 0;
               xfer = 0;
               start = 0;
               *ierr = 0;
               if ((*qcreat > 0) && (chunks > 2)) {
                  start = chunks - 1;
                  bytes = start;
                  bytes = bytes * BUFSIZE;
                  if (lseek (fd, bytes, 0) == -1) {
                     *syserr = errno;
                     sprintf (msgbuf, "ZCREA2: SEEK ERROR = %d", errno);
                     zmsgwr_ (msgbuf, &n6);
                     *ierr = 3;
                     }
                  }
               if (*ierr == 0) {
               for (i = start; i < chunks && *ierr == 0; i++) {
                  if ((xfer = write (fd, buff, BUFSIZE)) == -1) {
                     *syserr = errno;
                     *ierr = 4;
                     sprintf (msgbuf, "ZCREA2: WRITE ERROR = %d", errno);
                     zmsgwr_ (msgbuf, &n6);
                     }
                                        /* Increment space reserved.  */
                  else {
                     bytes = bytes + xfer;
                                        /* Presume insufficient space */
                     if (xfer != BUFSIZE) {
                        *ierr = 3;
                        }
                     }
                  }
               }
                                        /* If no error so far, write  */
                                        /* the "rest" (if any).       */
               if (*ierr == 0 && rest > 0) {
                  if ((xfer = write (fd, buff, rest)) == -1) {
                     *syserr = errno;
                     *ierr = 4;
                     sprintf (msgbuf, "ZCREA2: WRITE ERROR = %d", errno);
                     zmsgwr_ (msgbuf, &n6);
                     }
                                        /* Increment space reserved.  */
                  else {
                     bytes = bytes + xfer;
                                        /* Presume insufficient space */
                     if (xfer != rest) {
                        *ierr = 3;
                        }
                     }
                  }
                                        /* Free up the memory of the  */
                                        /* temporary I/O buffer.      */
               free (buff);
               }
                                        /* Convert the final 8-bit    */
                                        /* byte file size to          */
                                        /* AIPS-blocks for return     */
            *asize = bytes / Z_nbpab / 512;
                                        /* Close the file.            */
            if (close (fd) == -1) {
               *syserr = errno;
               *ierr = 4;
               sprintf (msgbuf, "ZCREA2: CLOSE ERROR = %d", errno);
               zmsgwr_ (msgbuf, &n6);
               }
                                        /* Did we get what we asked   */
                                        /* for?                       */
                                        /* Presume insufficient space */
            else if (*asize != *ablocks) {
               *ierr = 3;
               }
                                        /* Any errors.                */
            if (*ierr != 0) {
                                        /* Delete the file.           */
               if (unlink (pathname) == -1) {
                  *syserr = errno;
                  *ierr = 4;
                  sprintf (msgbuf, "ZCREA2: DELETE ERROR = %d", errno);
                  zmsgwr_ (msgbuf, &n6);
                  }
               }
            }
                                        /* Close space lock file.     */
         if (close (lockfd) == -1) {
            *syserr = errno;
            *ierr = 4;
            sprintf (msgbuf, "ZCREA2: CLOSE ERROR ON %s", lname);
            zmsgwr_ (msgbuf, &n6);
            }
         }
      }

   return;
}
