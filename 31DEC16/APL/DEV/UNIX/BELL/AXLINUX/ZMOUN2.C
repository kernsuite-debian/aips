#define Z_moun2__

#define MT_Debug 1                      /* debug message control      */

                                        /* Must not claim POSIX_SOURCE*/
                                        /* types.h would not define   */
                                        /* things needed in mtio.h    */
#ifdef _POSIX_SOURCE
#undef _POSIX_SOURCE
#endif

#include <unistd.h>
#include <stdlib.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <sys/file.h>
#include <string.h>
                                        /* Useful macros              */
#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif
#include <AIPSPROTO.H>
                                        /* For patch use (15OCT96).   */
#ifndef MAXEHEX
#define MAXEHEX 36
#endif

#if __STDC__
   void zmoun2_(int *mount, int *idrive, int *idens, int *ntaped,
      int *npops, int *nuser, char *msg, int *syserr, int *ierr)
#else
   zmoun2_(mount, idrive, idens, ntaped, npops, nuser, msg, syserr,
	   ierr)
   int *mount, *idrive, *idens, *ntaped, *npops, *nuser, *syserr, *ierr;
   char msg[];
#endif
/*--------------------------------------------------------------------*/
/*! mount or dismount magnetic tape device - lowest level Z           */
/*# Tape Z2                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997-2000                                    */
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
/*  Issue a software tape mount or dismount requests.                 */
/*  Inputs:                                                           */
/*     mount    I      Operation indicator code:                      */
/*                        1 => mount                                  */
/*                        0 => dismount                               */
/*     idrive   I      Drive number                                   */
/*     idens    I      Density                                        */
/*     ntaped   I      Number of tape drives                          */
/*     npops    I      Pops number                                    */
/*     nuser    I      User AIPS number                               */
/*  Output:                                                           */
/*     msg      C*80   Error or mount type message buffer             */
/*     syserr   I      System error code: 0 => no error               */
/*     ierr     I      Error return code: 0 => no error               */
/*                        1 => error                                  */
/*                        2 => device doesn't exist                   */
/*                        3 => i/o error to device                    */
/*                        4 => device already allocated               */
/*                        6 => other device open errors               */
/*                                                                    */
/*  JAU 97/Feb/01: Linux DAT and Exabyte version.                     */
/*  Modified to set/restore tape block size--needed for Exabytes.     */
/*--------------------------------------------------------------------*/
{
   static int fd;
   int    jerr, llen, xlen, xlnb, tfd, density, ir;
   struct mtop top;
   struct mtget stat;
   char   command[40], da00[5], lockfile[MAXAPATH+7], tape[7],
          xlated[MAXAPATH], densities[4];
   struct flock lock;
                                        /* Gleaned from mt.c in the */
                                        /* mt-st-0.4 distribution.  */
   struct {
      int code;
      char *name;
   } dens_tbl[] = {
                                        /* Not a comprehensive list; */
                                        /* only types AIPS knows it  */
                                        /* can use. */
      {0x13, "DDS"},
      {0x14, "EXB-8200"},
      {0x15, "EXB-8500"},
      {0x24, "DDS-2"},
      {0x8C, "EXB-8505 compressed"},    /* 140 */
      {0x90, "EXB-8205 compressed"},    /* 144 */
      {-1, NULL}
   };
   static int blocksize_detected[MAXEHEX];
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *syserr = 0;
   errno = 0;
   sprintf (msg, "   ");
   sprintf (tape, "TAPE%x", *idrive);
   densities[0] = 'l';
   densities[1] = 'm';
   densities[2] = 'h';
   densities[3] = 'h';
                                        /* Form the lock file name    */
   llen = 5;
   xlen = MAXAPATH;
   sprintf (da00, "DA00");
   ztrlo2_ (&llen, da00, &xlen, xlated, &xlnb, &jerr);
   if (jerr != 0) {
      sprintf (msg, "ZMOUN2: Couldn't translate logical %s", da00);
      *ierr = 1;
      goto exit;
      }
   xlated[xlnb] = '\0';
   sprintf (lockfile, "%s/%s.lock", xlated, tape);
                                        /* Translate logical TAPEn    */
   sprintf (tape, "AMT0%x", *idrive);
   llen = 5;
   xlen = MAXAPATH;
   ztrlo2_ (&llen, tape, &xlen, xlated, &xlnb, &jerr);
   if (jerr != 0) {
      sprintf (msg, "ZMOUN2: Couldn't translate logical %s", tape);
      *ierr = 1;
      goto exit;
      }
   xlated[xlnb] = '\0';
                                        /* MOUNT                      */
   if (*mount == 1) {
                                        /* Create/open the lock file  */
      if ((fd = open (lockfile, O_RDWR | O_CREAT, 0666)) == -1) {
         sprintf (msg, "ZMOUN2: Couldn't open lock file %s", lockfile);
         *syserr = errno;
         *ierr = 1;
         goto exit;
         }
#if MT_Debug                              /* NOTE - debug            */
      printf ("MOUN2: opened lock file %s\n", lockfile);
#endif
                                        /* Try to put a lock on it    */
                                        /* Apply non-blocking lock.   */
      lock.l_whence = SEEK_SET;
      lock.l_start = 0;
      lock.l_len = 0;
                                        /* Exclusive lock.            */
      lock.l_type = F_WRLCK;
                                        /* Shared lock                */
   /* lock.l_type = F_RDLCK; */

      if ((ir = fcntl (fd, F_SETLK, &lock)) == -1) {
#if MT_Debug                              /* NOTE - debug            */
         printf ("MOUN2: POSIX fcntl -1 errno = %d\n", errno);
#endif
         if ((errno == EACCES) || (errno == EAGAIN)) {
            sprintf (msg,
               "(POSIX lock) Tape %d is currently in use.", *idrive);
            *ierr = 4;
            }
         else {                       /* Some other error           */
            sprintf (msg, "POSIX lockfile error %d", errno);
            *ierr = 1;
            *syserr = errno;
            }
         goto cleanup;
         }
#if MT_Debug                              /* NOTE - debug            */
      else {
         printf ("MOUN2: POSIX fcntl return = %d errno = %d\n",
            ir, errno);
         }
#endif
                                        /* open root device name      */
      if ((strncmp(xlated, "on-line", 7) == 0) ||
         (strncmp(xlated, "ON-LINE", 7)) == 0) {
         sprintf (msg, "Mounted on-line tape device");
         }
      else {
         tfd = open (xlated, O_RDONLY | O_NONBLOCK) ;
         if (tfd == -1) {
                                        /* Device doesn't exist.      */
            if (errno == ENOENT)
               *ierr = 2;
                                        /* Some other open error.     */
            else
               *ierr = 6;
            *syserr = errno;
            sprintf (msg, "ZMOUN2: Couldn't open tape device %s",
               xlated);
            goto cleanup;
            }
         top.mt_count = 1;
         top.mt_op = MTNOP;
         if (ioctl(tfd,MTIOCTOP,&top) != 0) {
            *ierr = 3;
            sprintf(msg,
               "ZMOUN2: error performing null op on tape drive");
            goto cleanup;
            }
         if (ioctl(tfd,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            sprintf(msg, "ZMOUN2: error getting drive status");
            goto cleanup;
            }
         density = 0;
         if (*idens == 800) density = 1;
         if (*idens == 1600) density = 2;
         if (*idens == 6250) density = 3;
         if (*idens == 22500) density = 4;
                                        /* rename AMT0n on type/dens  */
         switch (stat.mt_type) {
            case MT_ISDDS1 :
               density = 0;
               sprintf (msg, "Mounted on standard DAT tape");
               break;
            case MT_ISDDS2 :
               density = 0;
               sprintf (msg, "Mounted on partitioned DAT tape - good luck");
               break;
            case MT_ISSCSI1 :
               density = 0;
               sprintf (msg, "Mounted on ANSI SCSI 1 tape");
               break;
            case MT_ISSCSI2 :
               density = 0;
               sprintf (msg, "Mounted on ANSI SCSI 2 tape");
               break;
            default :
               density = 0;
               sprintf (msg, "Mounted SCSI tape device type %x, a ????",
                  stat.mt_type);
               break;
            }

         /*
          * If it's a SCSI 1 or SCSI 2 tape drive, let's find out, and
          * display, a bit more information.  We'll also save the
          * current blocksize, set it to zero for the session, and
          * restore the old value upon dismount.  This routine was also
          * inspired by Linux's mt-st-0.4 distribution.
          */
         if (stat.mt_type == MT_ISSCSI1 || stat.mt_type == MT_ISSCSI2) {
            int dens_detected = (stat.mt_dsreg & MT_ST_DENSITY_MASK) >>
               MT_ST_DENSITY_SHIFT;
            if (blocksize_detected[*idrive] =
                ((stat.mt_dsreg & MT_ST_BLKSIZE_MASK) >> MT_ST_BLKSIZE_SHIFT)) {
#if MT_Debug
               printf ("ZMOUN2: Tape blocksize detected: %d, drive %d\n",
                       blocksize_detected[*idrive], *idrive);
#endif
               top.mt_op = MTSETBLK;
               top.mt_count = 0;
               if (ioctl (tfd, MTIOCTOP, &top) != 0) {
                  *ierr = 3;
                  sprintf (msg, "ZMOUN2: error setting tape block size to 0");
                  goto cleanup;
               }
            }
            if (dens_detected) {
               int dens_counter = 0;
               for (dens_counter; dens_tbl[dens_counter].code >= 0;
                    dens_counter++) {
                  if (dens_tbl[dens_counter].code == dens_detected) {
                     /* Watch buffer length! */
                     strcat (msg, " (");
                     strcat (msg, dens_tbl[dens_counter].name);
                     strcat (msg, ")");
                     break;
                  }
               }
            }
         }
         close (tfd);
                                        /* reset logical              */
         if (density) {
            xlated[xlnb-1] = densities[density-1];
            zcrlog_(&llen, tape, &xlnb, xlated, &jerr);
            if (jerr != 0) {
               sprintf (msg,
                  "ZMOUN2: Couldn't create logical %s", xlated);
               *ierr = 1;
               goto cleanup;
               }
            }
         }
      }
                                        /* DISMOUNT                   */
   else {
                                        /* Take the Exabyte offline */
      if ((strncmp(xlated, "on-line", 7) != 0) &&
         (strncmp(xlated, "ON-LINE", 7)) != 0) {
         tfd = open (xlated, O_RDONLY | O_NONBLOCK) ;
         if (tfd == -1) {
                                        /* Device doesn't exist.      */
            if (errno == ENOENT)
               *ierr = 2;
                                        /* Some other open error.     */
            else
               *ierr = 6;
            *syserr = errno;
            sprintf (msg, "ZMOUN2: Couldn't open tape device %s",
               xlated);
            goto cleanup;
            }
         /* Now lets set the tape block size back the way we found it. */
         if (blocksize_detected[*idrive]) {
#if MT_Debug
            printf ("ZMOUN2: Resetting tape block size back to %d, drive %d.\n",
                    blocksize_detected[*idrive], *idrive);
#endif
            top.mt_op = MTSETBLK;
            top.mt_count = blocksize_detected[*idrive];
            /* Failure is non-fatal, but we'll warn about it if debugging. */
#if MT_Debug
            if (ioctl (tfd, MTIOCTOP, &top) != 0) {
               fprintf (stderr,
                        "ZMOUN2: error resetting tape block size to %d, drive %d!\n",
                        blocksize_detected[*idrive], *idrive);
            }
#else
            ioctl (tfd, MTIOCTOP, &top);
#endif
         }
         top.mt_count = 1;
#if MT_Debug
         /* Don't eject the tape when debugging--it's damned annoying! */
         top.mt_op = MTREW;
#else
         top.mt_op = MTOFFL;
#endif
         if (ioctl(tfd,MTIOCTOP,&top) != 0) {
            *ierr = 3;
            sprintf(msg,
               "ZMOUN2: error performing Offline op on tape drive");
            goto cleanup;
            }
         close (tfd) ;
         }
cleanup:
                                        /* Release the lock file      */
      unlink (lockfile);
      close (fd);
      }

exit:
   return;
}

/*
 * Local Variables:
 * c-basic-offset: 3
 * comment-column: 40
 * indent-tabs-mode: nil
 * End:
 */
