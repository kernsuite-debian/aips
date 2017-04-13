#define Z_moun2__
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

/* from <scsi/targets/stdef.h> */
#define	ST_TYPE_HIC		0x26	/* Generic 1/2" Cartridge     */
#define	ST_TYPE_REEL		0x27	/* Generic 1/2" Reel Tape     */


#if __STDC__
   void zmoun2_(int *mount, int *idrive, int *idens, int *ntaped,
      int *npops, int *nuser, char *msg, int *syserr, int *ierr)
#else
   void zmoun2_(mount, idrive, idens, ntaped, npops, nuser, msg,
      syserr, ierr)
   int *mount, *idrive, *idens, *ntaped, *npops, *nuser, *syserr, *ierr;
   char msg[];
#endif
/*--------------------------------------------------------------------*/
/*! mount or dismount magnetic tape device - lowest level Z           */
/*# Tape Z2                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 1999-2000                              */
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
/*  MRC 90/May/22: SUN exabyte version.                               */
/*  Modified to not dismount "on-line" tapes at the VLA               */
/*  Modified for DLT support (device 51) PPM 1997.11.18               */
/*--------------------------------------------------------------------*/
{
   static int fd;
   int    jerr, llen, xlen, xlnb, tfd, density, ir;
   struct mtop top;
   struct mtget stat;
   struct mtdrivetype_request drvreq;
   struct mtdrivetype drvtyp;
   char   command[40], da00[5], lockfile[MAXAPATH+7], tape[7],
          xlated[MAXAPATH], densities[4];
   struct flock lock;
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
/*                                         DEBUG
      printf ("MOUN2: opened lock file %s\n", lockfile);              */
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
/*                                         DEBUG
         printf ("MOUN2: POSIX fcntl -1 errno = %d\n", errno);       */
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
/*                                         DEBUG
      else {
         printf ("MOUN2: POSIX fcntl return = %d errno = %d\n",
            ir, errno);
         }                                                           */
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
	 drvreq.size = sizeof(drvtyp);
	 drvreq.mtdtp = &drvtyp;
	 if (ioctl(tfd,MTIOCGETDRIVETYPE,&drvreq) != 0) {
            *ierr = 3;
            sprintf(msg, "ZMOUN2: error getting drive vendor info");
            goto cleanup;
            }
         close (tfd) ;
         density = 0;
         if (*idens == 800) density = 1;
         if (*idens == 1600) density = 2;
         if (*idens == 6250) density = 3;
         if (*idens == 22500) density = 4;
                                        /* rename AMT0n on type/dens  */
         switch (stat.mt_type) {
#ifdef MT_ISXY
            case MT_ISXY :
               density = min (density, 3);
               sprintf (msg, "Mounted on Xylogics 472 tape drive %s",
                        drvtyp.vid);
               break;
#endif
            case MT_ISKENNEDY :
               density = min (density, 3);
               sprintf (msg, "Mounted on Kennedy reel tape drive %s",
                        drvtyp.vid);
               break;
            case MT_ISHP :
               density = min (density, 3);
               sprintf (msg,
                        "Mounted on HP 1/2-inch reel tape drive %s",
                        drvtyp.vid);
               break;
            case ST_TYPE_HIC :
               density = min (density, 3);
               sprintf (msg,
                        "Mounted on Generic 1/2-inch Cartridge Tape %s",
                        drvtyp.vid);
               break;
            case ST_TYPE_REEL :
               density = min (density, 3);
               sprintf (msg, "Mounted on Generic 1/2-inch Reel Tape %s",
                        drvtyp.vid);
               break;
            case MT_ISEXB8500 :
               density = density / 4;
               if (density) {
                                        /* was 3, need m to avoid */
					/* getting compression mode */
                  density = 2;
                  sprintf (msg,
                           "Mounted Exabyte 8500 %s at HIGH DENSITY",
                           drvtyp.vid);
                  }
               else
                  sprintf (msg,
                           "Mounted Exabyte 8500 %s at low density",
                           drvtyp.vid);
               break;
            case MT_ISEXABYTE :
               density = 0;
               sprintf (msg,
                        "Mounted on Exabyte 8200 tape drive %s",
                        drvtyp.vid);
               break;
#ifdef MT_ISCCS28
            case MT_ISCCS28 :
               density = 0;
               sprintf (msg, "Mounted on (CCS 28) DAT tape drive %s",
                        drvtyp.vid);
               break;
#endif
#ifdef MT_ISDAT
            case MT_ISDAT :
               density = 0;
               sprintf (msg, "Mounted on DAT tape drive %s",
                        drvtyp.vid);
               break;
#endif
#ifdef MT_IS8MM
            case MT_IS8MM :
               density = density / 4;
               if (density) {
                                        /* was 3, need m to avoid */
					/* getting compression mode */
                  density = 2;
                  sprintf (msg,
                   "Mounted generic Exabyte 8500 %s at HIGH DENSITY",
                           drvtyp.vid);
                  }
               else
                  sprintf (msg,
                   "Mounted generic Exabyte 8500 %s at low density",
                           drvtyp.vid);
               break;
#endif
#ifdef MT_ISREEL
                                        /* DLT's when configured */
                                        /* appear to be a REEL type */
                                        /* device (51, 33 hex). */
                                        /* This depends on how the */
                                        /* device is configured!  */
            case MT_ISREEL:
               if (strstr(drvtyp.vid, "DLT7000") != NULL) {
                  if (density == 4) {
                     sprintf (msg,
                      "Mounted %s at HIGH DENSITY", drvtyp.vid);
                  } else {
		     density = 1;
                     sprintf (msg,
                      "Mounted %s at low density", drvtyp.vid);
                  }
               } else {
                  density = 1;
                  sprintf (msg,
                           "Mounted REEL tape (DLT? %s) at low density",
                           drvtyp.vid);
               }
               break;
#endif
            default :
               density = 0;
               sprintf (msg,
                        "Mounted SCSI tape %s, type %d, a DAT ??",
                        drvtyp.vid, stat.mt_type);
               break;
            }
                                        /* reset logical              */
         if (density) {
            xlated[xlnb-2] = densities[density-1];
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
                                        /* Take the device offline */
      if ((strncmp(xlated, "on-line", 7) != 0) &&
         (strncmp(xlated, "ON-LINE", 7)) != 0) {
/*         sprintf (command, "mt -f %s offline", xlated);
         system (command); */
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
         top.mt_op = MTOFFL;
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
