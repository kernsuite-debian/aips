#include <errno.h>
#include <fcntl.h>
#include <sys/file.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/devio.h>
                                        /* Useful macros              */
#ifndef min
#define min(a,b) ((a)<(b)?(a):(b))
#endif

#define MAXLLEN   48                    /* Maximum "lognam" length.   */
#define MAXXLEN   128                   /* Maximum "xlated" length.   */

zmoun2_(mount, idrive, idens, ntaped, npops, nuser, msg, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! mount or dismount magnetic tape device - lowest level Z           */
/*# Tape Z2                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995,  2000                                        */
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
/*     idens    I      Density                                        */
/*     ntaped   I      Number of tape drives                          */
/*     npops    I      Pops number                                    */
/*     nuser    I      User AIPS number                               */
/*  Output:                                                           */
/*     msg      C*80   Error message buffer                           */
/*     syserr   I      System error code: 0 => no error               */
/*     ierr     I      Error return code: 0 => no error               */
/*                        1 => error                                  */
/*                                                                    */
/*  ULTRIX version (no 8500 Exabyte).                                 */
/*--------------------------------------------------------------------*/
int *mount, *idrive, *idens, *ntaped, *npops, *nuser, *syserr, *ierr;
char msg[];
/*--------------------------------------------------------------------*/
{
   extern int errno;
   static int fd;
   int    jerr, llen, xlen, xlnb, tfd, density, id, ir;
   struct mtop top;
   struct mtget stat;
   struct devget devs;
   char   command[40], da00[5], lockfile[MAXXLEN+7], tape[7],
          xlated[MAXXLEN], xlate2[MAXXLEN];
/*--------------------------------------------------------------------*/
   *ierr = 0;
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
                                        /* Translate logical TAPEn    */
   sprintf (tape, "AMT0%x", *idrive);
   llen = 5;
   xlen = MAXXLEN;
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
                                        /* Try to put a lock on it    */
      if (flock (fd, LOCK_EX | LOCK_NB) != 0) {
         if (errno == EWOULDBLOCK) {
            sprintf (msg, "Tape %d is currently in use.", *idrive);
            }
         else {
            sprintf (msg, "ZMOUN2: Couldn't lock %s", lockfile);
            *syserr = errno;
            }
         *ierr = 1;
         goto exit;
         }
                                        /* open root device name      */
      if (!strncasecmp(xlated, "ON-LINE", 7)) {
         sprintf (msg, "Mounted on-line tape device");
         }
      else {
         tfd = open (xlated, 0) ;
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
         if (ioctl(tfd,DEVIOCGET,&devs) != 0) {
            *ierr = 3;
            sprintf(msg, "ZMOUN2: error getting drive device status");
            goto cleanup;
            }
         close (tfd) ;
         density = 0;
         if (*idens == 1600) density = 1;
         if (*idens == 6250) density = 2;
         if (*idens == 22500) density = 3;
                                        /* rename AMT0n on type/dens  */
         switch (stat.mt_type) {
            case MT_ISTS :
               density = min (density, 2);
               sprintf (msg, "Mounted on TS (%s) tape drive",
                  devs.device);
               break;
            case MT_ISHT :
               density = min (density, 2);
               sprintf (msg, "Mounted on HT (%s) tape drive",
                  devs.device);
               break;
            case MT_ISTM :
               density = min (density, 2);
               sprintf (msg, "Mounted on TM (%s) tape drive",
                  devs.device);
               break;
            case MT_ISMT :
               density = min (density, 2);
               sprintf (msg, "Mounted on MT (%s) tape drive",
                  devs.device);
               break;
            case MT_ISUT :
               density = min (density, 2);
               sprintf (msg, "Mounted on UT (%s) tape drive",
                  devs.device);
               break;
            case MT_ISTMSCP :
               density = min (density, 2);
               sprintf (msg, "Mounted on TMSCP (%s) tape drive",
                  devs.device);
               break;
            case MT_ISST :
               density = min (density, 2);
               sprintf (msg, "Mounted on ST (%s) tape drive",
                  devs.device);
               break;
            case MT_ISSCSI :
               density = min (density, 2);
               if ((devs.category_stat & DEV_61000_BPI) != 0)
                  sprintf (msg, "Mounted on 4mm DAT (%s) tape drive",
                     devs.device);
               else if ((devs.category_stat & DEV_54000_BPI) != 0)
                  sprintf (msg, "Mounted on 8mm (%s) tape drive",
                     devs.device);
               else
                  sprintf (msg, "Mounted on SCSI (%s) tape drive",
                     devs.device);
               break;
            default :
               density = 0;
               sprintf (msg, "Mounted SCSI tape device type %d (%s)",
                  stat.mt_type, devs.device);
               break;
            }
         if (density) {
            xlated[xlnb-1] = 'l';
            if (density == 1) xlated[xlnb-1] = 'm';
            if (density == 2) xlated[xlnb-1] = 'h';
            zcrlog_(&llen, tape, &xlnb, xlated, &jerr);
            if (jerr != 0) {
               sprintf (msg, "ZMOUN2: Couldn't create logical %s",
                  xlated);
               *ierr = 1;
               goto cleanup;
               }
            }
         }
      }
                                        /* DISMOUNT                   */
   else {
                                        /* Take the Exabyte offline */
      sprintf (command, "mt -f %s offline", xlated);
      system (command);

cleanup:
                                        /* Release the lock file      */
      unlink (lockfile);
      close (fd);
      }

exit:
   return;
}
