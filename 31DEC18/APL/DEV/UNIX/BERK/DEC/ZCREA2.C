#include <stdio.h>
#include <strings.h>
#include <errno.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/param.h>
#include <sys/mount.h>
#define MAXPNAME 48                     /* Maximum logical device     */
                                        /* plus file name length.     */
#define MAXPATH 128                     /* Maximum path name length.  */
#define BUFSIZE 65536                   /* Buffer size for space      */
                                        /* reservation process.       */
#define PMODE 0666                      /* Creation permission bits.  */

#define SIZ_ST_STTT sizeof(struct statfs)

zcrea2_(pname, ablocks, asize, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! create the specified disk file                                    */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2000, 2003                             */
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
/*     asize    I      Actual size of file created in AIPS-bytes      */
/*     syserr   I      System error code                              */
/*     ierr     I      Error return code: 0 => no error               */
/*                        1 => file already exists                    */
/*                        2 => volume not found                       */
/*                        3 => insufficient space                     */
/*                        4 => other                                  */
/*  4.2 bsd UNIX version (uses "statfs"), modified for OSF/1 (Alpha)  */
/*  Can handle filesystems >2 Gigablocks, files up to 2 Gigablocks      */
/*--------------------------------------------------------------------*/
char pname[MAXPNAME];
int *ablocks, *asize, *syserr, *ierr;
/*--------------------------------------------------------------------*/
{
                                        /* # bytes per AIPS-byte      */
   extern int Z_nbpab;
   int llen, xlen, xlnb, jerr, fd, tries, lockfd, tdelay,
      iexcl = 1, n6 = 6, n8 = 8;
   int slens;
   off_t bytes;
   int blocks, chunks, rest, xfer;
   register int i, j, k;
   float delay = 2.0;
   char lognam[MAXPNAME], xlated[MAXPATH], filename[MAXPNAME],
      lname[MAXPNAME], lockname[MAXPATH], pathname[MAXPATH], msgbuf[80];
   char cmd[128];
   char *buff = NULL;
   struct stat statbuf;
   struct statfs statfsbuf;
/*--------------------------------------------------------------------*/
   slens = SIZ_ST_STTT;
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
   bytes = *ablocks * Z_nbpab;
   bytes = bytes * 512;
                                        /* Translate logical device   */
                                        /* name and null terminate.   */
   llen = MAXPNAME;
   xlen = MAXPATH;
   ztrlo2_ (&llen, lognam, &xlen, xlated, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 2;
      sprintf (msgbuf, "ZCREA2: TROUBLE TRANSLATING LOGICAL = %s",
         lognam);
      zmsgwr_ (msgbuf, &n6);
      }
   else if (bytes <= 0) {
      *ierr = 3;
      sprintf (msgbuf, "ZCREA2: BYTES OVERFLOW ? = %d", bytes);
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
                                        /* blocks                     */
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
                     "ZCREA2: ERROR CREATING SPACE LOCK FILE = %s",
                        lname);
                  zmsgwr_ (msgbuf, &n8);
                  goto exit;
               }
            }
            else {
                                        /* Some other open error.     */
               *ierr = 4;
               sprintf (msgbuf,
                  "ZCREA2: ERROR OPENING SPACE LOCK FILE = %s", lname);
               zmsgwr_ (msgbuf, &n8);
               goto exit;
            }
         }
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
         else if (jerr != 0) {
                                        /* Some other error locking   */
                                        /* space lock file.           */
            sprintf (msgbuf,
               "ZCREA2: ERROR LOCKING SPACE LOCK FILE = %s", lname);
            zmsgwr_ (msgbuf, &n8);
            sprintf (msgbuf,
               "ZCREA2: PERFORMING NON-ATOMIC FILE CREATION INSTEAD");
            zmsgwr_ (msgbuf, &n8);
         }
                                        /* Get 4.2bsd block size for  */
                                        /* target file system.        */
         if (stat (xlated, &statbuf) == -1) {
            *syserr = errno;
            *ierr = 4;
            perror ("ZCREA2 (stat)");
         }
                                        /* Get free space info for    */
                                        /* target file system.        */
	                                /* OSF/1: statfs() needs len! */
         else if (statfs (xlated, &statfsbuf, slens) == -1) {
            *syserr = errno;
            *ierr = 4;
            perror ("ZCREA2 (statfs)");
         }
                                        /* Is there sufficient space? */
         else {
                                        /* Convert number of AIPS-    */
                                        /* blocks requested to local  */
                                        /* disk block units.          */
            bytes = *ablocks * Z_nbpab;
            bytes = bytes * 512;
            blocks = 1 + (bytes-1) / statfsbuf.f_bsize ;
                                        /* Enough room?               */
            if (statfsbuf.f_bavail < blocks) {
                                        /* Insufficient space.        */
               *ierr = 3;
               }
            else {
                                        /* Create the file.           */
               if ((fd = open (pathname, O_CREAT | O_EXCL | O_WRONLY,
                  PMODE)) == -1) {
                  if (errno == EEXIST) {
                                        /* File already exists.       */
                     *ierr = 1;
                  }
                  else {
                                        /* Some other open/creation   */
                                        /* error.                     */
                     *syserr = errno;
                     *ierr = 4;
                     sprintf (msgbuf, "ZCREA2: CREATE ERROR");
                     zmsgwr_ (msgbuf, &n6);
                  }
               }
               else {
                                        /* Apply non-blocking lock.   */
                  zlock_ (&fd, pname, &iexcl, syserr, &jerr);
                                        /* Allocate memory for a      */
                                        /* temporary I/O buffer.      */
                  if ((buff = (char *) malloc (BUFSIZE)) == NULL) {
                     *syserr = errno;
                     *ierr = 4;
                     sprintf
                        (msgbuf, "ZCREA2: MEMORY ALLOCATION ERROR");
                     zmsgwr_ (msgbuf, &n6);
                  }
                  else {
                     memset (buff, 0, BUFSIZE);
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
                     bytes = 0;
                     xfer = 0;
                     for (i = 0; i < chunks && *ierr == 0; i++) {
                        if ((xfer = write (fd, buff, BUFSIZE)) == -1) {
                           *syserr = errno;
                           *ierr = 4;
                           sprintf (msgbuf, "ZCREA2: WRITE ERROR");
                           zmsgwr_ (msgbuf, &n6);
                        }
                        else {
                                        /* Increment space reserved.  */
                           bytes = bytes + xfer;
                           if (xfer != BUFSIZE) {
                                        /* Presume insufficient       */
                                        /* space.                     */
                              *ierr = 3;
                           }
                        }
                     }
                                        /* If no error so far, write  */
                                        /* the "rest" (if any).       */
                     if (*ierr == 0 && rest > 0) {
                        if ((xfer = write (fd, buff, rest)) == -1) {
                           *syserr = errno;
                           *ierr = 4;
                           sprintf (msgbuf, "ZCREA2: WRITE ERROR");
                           zmsgwr_ (msgbuf, &n6);
                        }
                        else {
                                        /* Increment space reserved.  */
                           bytes = bytes + xfer;
                           if (xfer != rest) {
                                        /* Presume insufficient       */
                                        /* space.                     */
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
                     sprintf (msgbuf, "ZCREA2: CLOSE ERROR");
                     zmsgwr_ (msgbuf, &n6);
                  }
                                        /* Did we get what we asked   */
                                        /* for?                       */
                  else if (*asize != *ablocks) {
                                        /* Presume insufficient       */
                                        /* space.                     */
                     *ierr = 3;
                  }
                                        /* Any errors.                */
                  if (*ierr != 0) {
                                        /* Delete the file.           */
                     if (unlink (pathname) == -1) {
                        *syserr = errno;
                        *ierr = 4;
                        sprintf (msgbuf, "ZCREA2: DELETE ERROR");
                        zmsgwr_ (msgbuf, &n6);
                     }
                  }
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
                                        /* Exit.                      */
exit:
   return;
}
