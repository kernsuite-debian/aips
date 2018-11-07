#include <errno.h>
#include <fcntl.h>
/* #include <sys/file.h> */
/* #include <stdio.h> */

#ifdef _POSIX_SOURCE
#include <sys/types.h>
#include <unistd.h>
#include <limits.h>
#include <stdio.h>
#else
#include <sys/file.h>
#endif

#define MAXLLEN   48                    /* Maximum "lognam" length.   */
#define MAXXLEN   128                   /* Maximum "xlated" length.   */

                                        /* Useful macros              */
#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

zmoun2_(mount, idrive, idens, ntaped, npops, nuser, msg, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! mount or dismount magnetic tape device - lowest level Z           */
/*# Tape Z2                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 2000                                         */
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
/*  Issue a software tape mount or dismount requests.                 */
/*  Inputs:                                                           */
/*     mount    I      Operation indicator code:                      */
/*                        1 => mount                                  */
/*                        0 => dismount                               */
/*     idrive   I      Drive number                                   */
/*     idens    I      Density (not tested here)                      */
/*     ntaped   I      Number of tape drives                          */
/*     npops    I      Pops number                                    */
/*     nuser    I      User AIPS number                               */
/*  Output:                                                           */
/*     msg      C*80   Error or mount type message buffer             */
/*     syserr   I      System error code: 0 => no error               */
/*     ierr     I      Error return code: 0 => no error               */
/*                        1 => error                                  */
/*  IBM version - uses ZCRLOG to include the density                  */
/*--------------------------------------------------------------------*/
int *mount, *idrive, *idens, *ntaped, *npops, *nuser, *syserr, *ierr;
char msg[];
/*--------------------------------------------------------------------*/
{
   extern int errno;
   register int i, j;
   static int fd;
   int    jerr, llen, xlen, xlnb, vers, rev, density, ir;
   pid_t  pid0, pid1;
   char cmd[256], da00[5], mt0x[5], lockfile[MAXXLEN+7], tape[7],
      xlated[MAXXLEN], *chptr;
   FILE *mpipe;
   extern int IBM_taptyp;      /* 0-3 unknown,reel,Exabyte,DAT        */
#ifdef _POSIX_SOURCE
   struct flock lock;
#endif
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *syserr = 0;
   errno = 0;
   sprintf (msg, "   ");
   sprintf (tape, "TAPE%x", *idrive);
                                        /* Form the lock file name    */
   llen = 5;
   xlen = MAXXLEN;
   sprintf (da00, "DA00");
   ztrlo2_ (&llen, da00, &xlen, xlated, &xlnb, &jerr);
   if (jerr != 0) {
      sprintf (msg, "ZMOUN2: Couldn't translate logical %s", da00);
      *ierr = 1;
      goto exit;
      }
   xlated[xlnb] = '\0';
   sprintf (lockfile, "%s/%s.lock", xlated, tape);
                                        /* Translate logical TAPEn   */
   sprintf(mt0x,"AMT0%x",*idrive);
   llen = 5;
   xlen = MAXXLEN;
   ztrlo2_ (&llen, mt0x, &xlen, xlated, &xlnb, &jerr);
   if (jerr != 0) {
      sprintf (msg, "ZMOUN2: Couldn't translate logical %s", mt0x);
      *ierr = 1;
      goto exit;
      }
   xlated[xlnb] = '\0';
                                        /* MOUNT or DISMOUNT?         */
   if (*mount == 1) {
                                        /* Create/open the lock file  */
      if ((fd = open (lockfile, O_RDWR | O_CREAT, 0666)) == -1) {
         *syserr = errno;
         perror ("ZMOUN2 open lock");
         sprintf (msg, "ZMOUN2: Couldn't open lock file %s", lockfile);
         *ierr = 1;
         goto exit;
         }
                                        /* Try to put a lock on it    */
#ifdef _POSIX_SOURCE
                                        /* Apply non-blocking lock.   */
      lock.l_whence = SEEK_SET;
      lock.l_start = 0;
      lock.l_len = 0;
                                        /* Exclusive lock.            */
      lock.l_type = F_WRLCK;
                                        /* Shared lock                */
   /* lock.l_type = F_RDLCK; */

      if ((ir = fcntl (fd, F_SETLK, &lock)) == -1) {
         if ((errno == EACCES) || (errno == EAGAIN)) {
                                        /* see who owns the lock */
            if ( fcntl (fd, F_GETLK, &lock) == -1) {
               sprintf (msg,
                       "(POSIX lock) Tape %d is currently in use.",
                       *idrive);
               }
            else {
               sprintf (msg,
                       "(POSIX lock) Tape %d is in use by pid %d.",
                       *idrive, lock.l_pid);
               }
            *ierr = 4;
            }
         else {                       /* Some other error           */
            *syserr = errno;
            sprintf (msg, "ZMOUN2: can't lock %s; return = %d errno %d",
                     lockfile, ir, errno);
            *ierr = 1;
            }
         goto cleanup;
       }
#else
                                        /* rest is NOT _POSIX_SOURCE */
      if (flock (fd, LOCK_EX | LOCK_NB) != 0) {
         if (errno == EWOULDBLOCK) {
            sprintf (msg, "Tape %d is currently in use.", *idrive);
            }
         else {
            sprintf (msg, "ZMOUN2: Couldn't lock %s", lockfile);
            *syserr = errno;
            }
         *ierr = 1;
         goto cleanup;
         }
#endif
                                        /* Get tape device type       */
      xlated[xlnb] = '\0';
      chptr = xlated + 5;
      sprintf (cmd, "/etc/lsdev -C -r type -l %s", chptr);
      for (i = 0; i < sizeof(cmd); i++) {
         if (cmd[i] == '.') {
            cmd[i] = '\0' ;
            break ;
            }
         }
      density = 0;
      if (*idens == 1600) density = 1;
      if (*idens == 6250) density = 2;
      if (*idens == 22500) density = 3;
      if  ((mpipe = popen (cmd, "r")) == NULL) {
         sprintf (msg, "Mounted tape device of unknown type");
         IBM_taptyp = 0;
         }
      else {
         fgets (cmd, sizeof(cmd), mpipe);
         pclose (mpipe);
         j = 99;
         for (i = 0; i < 20; i ++) {
            if (cmd[i] == '\0') break ;
            if (cmd[i] > ' ') j = i;
            if (cmd[i] < ' ') {
               cmd[i] = '\0';
               break ;
               }
            }
         cmd[j+1] = '\0';
/*                                         DEBUG
         printf ("Device type returned = %s for tape\n", cmd);        */
         if (strcmp (cmd, "9trk") == 0) {
            sprintf (msg, "Mounted on 1/2-inch reel tape drive");
            IBM_taptyp = 1;
            density = min (density, 2) ;
            }
         else if (strcmp (cmd, "8mm") == 0) {
            IBM_taptyp = 2;
/*          density = 0;                Temp, can't tell from 8500 */
/*                                      Next is temporary too...   */
            if (density == 3) {
               sprintf (msg, "Mounted on Exabyte 8200 (or 8500 HIGH DENSITY)");
               }
            else {
               sprintf (msg, "Mounted on Exabyte 8200 (or 8500 low density)");
               density = 1;
               }
            }
         else if (strcmp (cmd, "8mm5gb") == 0) {
            IBM_taptyp = 2;
            if (density == 3) {
               sprintf (msg, "MOUNTED Exabyte 8500 at HIGH DENSITY");
               }
            else {
               sprintf (msg, "Mounted Exabyte 8500 at low density");
               density = 1;
               }
            }
         else if (strcmp (cmd, "150mb") == 0) {
            sprintf (msg,
               "Mounted on DAT (pseudo cartridge) tape drive");
            IBM_taptyp = 3;
            density = 0;
            }
         else if (strcmp (cmd, "ost") == 0) {
            sprintf (msg, "Mounted scsi tape device of unknown type");
            IBM_taptyp = 0;
            }
         else {
            sprintf (msg, "Mounted tape device of unknown type");
            IBM_taptyp = 0;
            }
         }
                                        /* Assign shell variable AMT0n*/
                                        /* "/dev/rmtMM"               */
      xlated[xlnb] = '.';
      if (density >= 2)
         xlated[xlnb+1] = '1';
      else
         xlated[xlnb+1] = '5';
      xlated[xlnb+2] = '\0';
      xlnb += 2;
      zcrlog_(&llen, mt0x, &xlnb, xlated, &jerr);
      if (jerr != 0) {
         sprintf (msg, "ZMOUN2: Couldn't create logical %s", xlated);
         *ierr = 1;
         goto cleanup;
         }
      }
                                        /* DISMOUNT                   */
   else {
      xlated[xlnb] = '\0';
      if (xlated[xlnb-2] == '.') xlated[xlnb-2] = '\0';

                                        /* Rewind the tape, but check */
                                        /* first for AIX version; the */
                                        /* offline command is only    */
                                        /* supported in AIX 3.2       */
      if ((mpipe = popen ("/bin/uname -r -v", "r")) == NULL ) {
         sprintf (cmd, "rewind");
         }
      else {
         fgets (cmd, sizeof(cmd), mpipe);
         pclose(mpipe);
         sscanf (cmd, "%d %d", &rev, &vers);
         if (((vers == 3) && (rev >= 2)) || (vers > 3)) {
            sprintf (cmd, "offline");
            }
         else {
            sprintf (cmd, "rewind");
            }
         }
                                        /* We are the child           */
      if ((pid0 = fork()) == 0) {
         execl ("/bin/tctl", "AIPSmt", "-f", xlated, cmd,
                (char *) 0);
         fprintf (stderr, "ZMOUN2: Should never get here!\n");
         }
                                        /* We're the parent          */
      else {
         if (errno == 0) {
            pid1 = wait ((void *) 0);
            }
         }
/*      system (cmd);     */
cleanup:
                                        /* Release the lock file      */
      unlink (lockfile);
      close (fd);
      }

exit:
   return;
}

