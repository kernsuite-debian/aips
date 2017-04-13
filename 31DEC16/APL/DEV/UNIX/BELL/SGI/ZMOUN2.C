#define Z_moun2__

/* Define MT_Debug to be non-zero to obtain debugging messages */
#define MT_Debug 0

#include <unistd.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <string.h>
#include <sys/types.h>
#include <sys/invent.h>
#include <sys/mtio.h>

#include <AIPSPROTO.H>

void zmoun2_(int *mount, int *idrive, int *idens, int *ntaped,
             int *npops, int *nuser, char *msg, int *syserr, int *ierr)
/*--------------------------------------------------------------------*/
/*! mount or dismount magnetic tape device - lowest level Z           */
/*# Tape Z2                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997, 2000                                    */
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
/*     idens    I      Density (assumed to be already validated)      */
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
/*  IRIX 6 version.  Tape device names should have the form           */
/*  /dev/rmt/tps{ctrlr}d{id}nrv{.density} where {ctrlr} is the        */
/*  controller number, {id} is the device id number and {.density} is */
/*  an optional suffix denoting the density.  The suffix is added     */
/*  automatically.                                                    */
/*--------------------------------------------------------------------*/
{
   struct mtop tape_operation;
   struct mtget tape_status;
   struct mt_capablity tape_capability;
   struct flock lock;

   char da00[MAXAPATH+1];               /* pathname of DA00 directory */
   char lockfile[MAXAPATH+14];          /* lockfile name              */
   char tapename[7];                    /* logical holding tape       */
                                        /* device name                */
   char tape_device[MAXAPATH+1];        /* tape device name           */

   int logical_length;                  /* length of logical name     */
   int logical_buffer_length;           /* length of buffer to hold   */
                                        /* expanded logical name      */
   int expanded_length;                 /* length of expanded logical */
                                        /* name                       */

   static int lockfd = 0;               /* file descriptor for lock   */
                                        /* file                       */
   int tapefd = 0;                      /* file descriptor for tape   */
                                        /* device                     */

   char *suffix;                        /* index of suffix in device  */
                                        /* name                       */

   int status;
   int i;
/*--------------------------------------------------------------------*/
   /*
    * Initialize default return values:
    */

   *ierr = 0;
   *syserr = 0;
   errno = 0;
   memset(msg, (int) ' ', (size_t) 80);
   msg[80] = '\0';

   /*
    * Expand DA00:
    */

   logical_length = 4;
   logical_buffer_length = MAXAPATH;
   ztrlo2_(&logical_length, "DA00", &logical_buffer_length, da00,
           &expanded_length, ierr);
   if (*ierr != 0)
   {
      sprintf(msg, "ZMOUN2: could not translate logical DA00");
      *ierr = 1;
   }
   else
   {
      da00[expanded_length] = '\0';
   }

   if (*ierr == 0)
   {
      /*
       * Generate lockfile name from the drive number:
       */

      sprintf(lockfile, "%s/TAPE%x.lock", da00, *idrive);

      /*
       * Find the device name for the tape drive:
       */

      sprintf(tapename, "AMT0%x", *idrive);
      logical_length = strlen(tapename);
      logical_buffer_length = MAXAPATH;
      ztrlo2_(&logical_length, tapename, &logical_buffer_length, tape_device,
              &expanded_length, ierr);
      if (*ierr != 0)
      {
         sprintf(msg, "ZMOUN2: could not translate logical %s", tapename);
         *ierr = 1;
      }
      else
      {
         tape_device[expanded_length] = '\0';
      }
   }

   if (*mount)                          /* Mount the tape             */
   {
      if (*ierr == 0)
      {
         /*
          * Create and open the lock file:
          */

         lockfd = open(lockfile, O_RDWR | O_CREAT, 0666);
         if (lockfd == -1)
         {
            sprintf(msg, "ZMOUN2: could not open lock file %s", lockfile);
            *syserr = errno;
            *ierr = 1;
         }
#if MT_Debug
         else
         {
            printf("ZMOUN2: opened lock file %s\n", lockfile);
         }
#endif
      }
                                        /* From this point on in the  */
                                        /* mount branch, the lockfile */
                                        /* is open and must be closed */
                                        /* and deleted on errors.     */
      if (*ierr == 0)
      {
         /*
          * Acquire an exclusive lock on the lock file:
          */

         lock.l_whence = SEEK_SET;
         lock.l_start  = 0;
         lock.l_len    = 0;
         lock.l_type   = F_WRLCK;
         status = fcntl(lockfd, F_SETLK, &lock);
         if (status != 0)
         {
#if MT_Debug
            perror("ZMOUN2 fcntl(lockfd)");
#endif
            switch (errno)
            {
            case EACCES:
            case EAGAIN:
               sprintf(msg, "ZMOUN2: tape %d is already in use.", *idrive);
               *ierr = 4;
               break;
            default:
               sprintf(msg, "ZMOUN2: errno = %d locking file", errno);
               *ierr = 1;
               break;
            }
            close(lockfd);
            unlink(lockfile);
         }
#if MT_Debug
         else
         {
            printf("ZMOUN2: acquired exclusive lock\n");
         }
#endif
      }
                                        /* From this point on in the  */
                                        /* mount branch, there is an  */
                                        /* exclusive lock on the lock */
                                        /* file which must be         */
                                        /* relinquished on errors.    */

      lock.l_type = F_UNLCK;

      if (*ierr == 0)
      {
         /*
          * Open the tape device:
          */

          if (strncmp(tape_device, "on-line", 7) == 0
              || strncmp(tape_device, "ON-LINE", 7) == 0)
          {
                                        /* nothing else to do ---     */
                                        /* not a real device          */
             sprintf(msg, "Mounted on-line tape device");
          }
          else
          {
             tapefd = open(tape_device, O_RDONLY | O_NONBLOCK);
             if (tapefd == -1)
             {
                switch (errno)
                {
                case ENOENT:
                   *ierr = 2;
                   break;
                default:
                   *ierr = 6;
                }
                *syserr = errno;
                sprintf(msg, "ZMOUN2: Could not open tape device %s",
                        tape_device);
                fcntl(lockfd, F_SETLK, &lock);
                close(lockfd);
                unlink(lockfile);
             }

             if (*ierr == 0)
             {
                /*
                 * Issue a null operation so that we can read back the
                 * device status:
                 */

                tape_operation.mt_count = 1;
                tape_operation.mt_op    = MTNOP;
                status = ioctl(tapefd, MTIOCTOP, &tape_operation);
                if (status != 0)
                {
                   *ierr = 3;
                   *syserr = errno;
                   sprintf(msg,
                           "ZMOUN2: error performing null op on drive");
                   fcntl(lockfd, F_SETLK, &lock);
                   close(lockfd);
                   unlink(lockfile);
                }
             }

             if (*ierr == 0)
             {
                /*
                 * Read back the drive status:
                 */

                status = ioctl(tapefd, MTIOCGET, &tape_status);
                if (status != 0)
                {
                   *ierr = 3;
                   *syserr = errno;
                   sprintf(msg,
                           "ZMOUN2: error reading drive status");
                   fcntl(lockfd, F_SETLK, &lock);
                   close(lockfd);
                   unlink(lockfile);
                }
             }

             if (*ierr == 0)
             {
                /*
                 * Read the drive capabilities:
                 */

                status = ioctl(tapefd, MTCAPABILITY, &tape_capability);
                if (status != 0)
                {
                   *ierr = 3;
                   *syserr = errno;
                   sprintf(msg,
                           "ZMOUN2: error reading drive status");
                   fcntl(lockfd, F_SETLK, &lock);
                   close(lockfd);
                   unlink(lockfile);
                }
             }

             if (*ierr == 0)
             {
                /*
                 * Suppress the illegal-length indicator so that
                 * short reads/writes will not be reported as
                 * errors:
                 */

                tape_operation.mt_op = MTSCSI_SILI;
                status = ioctl(tapefd, MTSPECOP, &tape_operation);
                if (status != 0)
                {
                   *ierr = 3;
                   *syserr = errno;
                   sprintf(msg,
                           "ZMOUN2: error suppressing ILI");
                   fcntl(lockfd, F_SETLK, &lock);
                   close(lockfd);
                   unlink(lockfile);
                }
             }
                                        /* Finished with device.      */

             close(tapefd);

#if MT_Debug
             printf("ZMOUN2: mt_dposn %x mt_dsreg %x mt_erreg %x\n",
                    tape_status.mt_dposn, tape_status.mt_dsreg,
                    tape_status.mt_erreg);
             printf("ZMOUN2: mt_type %x mtsubtype %x mtcapability %x\n",
                    tape_status.mt_type, tape_capability.mtsubtype,
                    tape_capability.mtcapablity);
#endif

             if (*ierr == 0)
             {
                /*
                 * Change the generic type name to the one specific to
                 * the density requested (depends on device type):
                 */


                /*
                 * First remove any suffix:
                 */

                suffix = strrchr(tape_device, '.');
                if (suffix != NULL)
                {
                   *suffix = '\0';
                }

                                        /* There should be enough     */
                                        /* space to append the den-   */
                                        /* sity suffix to the device  */
                                        /* name so there is no need   */
                                        /* to check.                  */

                switch (tape_status.mt_type)
                {
                case MT_ISSCSI:
                   switch (tape_capability.mtsubtype)
                   {
                   case TPDAT:
                                        /* No density switches for    */
                                        /* DAT                        */
                      sprintf(msg, "Mounted on DAT drive");
                      break;
                   case TP9TRACK:
                      switch (*idens)
                      {
                      case 6250:
                         strcat(tape_device, ".6250");
                         break;
                      case 1600:
                         strcat(tape_device, ".1600");
                         break;
                      case 800:
                         strcat(tape_device, ".800");
                         break;
                      default:
                         /*
                          * Use generic name (is this correct?):
                          */
                         break;
                      }
                      sprintf(msg, "Mounted on 9-track tape drive");
                   case TP8MM_8200:
                      strcat(tape_device, ".8200");
                      sprintf(msg, "Mounted on Exabyte 8200 drive");
                      break;
                   case TP8MM_8500:
                      if (*idens == 22500)
                      {
                         strcat(tape_device, ".8500");
                         sprintf(msg,
                                 "Mounted on Exabyte 8500 (high density)");
                      }
                      else
                      {
                         strcat(tape_device, ".8200");
                         sprintf(msg,
                                 "Mounted on Exabyte 8500 (low density)");
                      }
                      break;
                   default:
                      sprintf(msg, "Unsupported SCSI drive type %x",
                              tape_capability.mtsubtype);
                      *ierr = 2;
                      fcntl(lockfd, F_SETLK, &lock);
                      close(lockfd);
                      unlink(lockfile);
                      break;
                   }
                   break;
                default:
                   sprintf(msg, "Unsupported controller type %x",
                           tape_status.mt_type);
                   *ierr = 2;
                   fcntl(lockfd, F_SETLK, &lock);
                   close(lockfd);
                   unlink(lockfile);
                   break;
                }
             }

             if (*ierr == 0)
             {
                expanded_length = strlen(tape_device);
                zcrlog_(&logical_length, tapename, &expanded_length,
                        tape_device, ierr);
                if (*ierr != 0)
                {
                   sprintf(msg, "ZMOUN2: failed to create logical %s",
                           tapename);
                   *ierr = 1;
                   *syserr = errno;
                   fcntl(lockfd, F_SETLK, &lock);
                   close(lockfd);
                   unlink(lockfile);
                }
             }
          }
       }
                                        /* If *ierr == 0 then the     */
                                        /* tape is mounted and the    */
                                        /* lock file is open and      */
                                        /* locked by this process.    */
   }
   else                                 /* Unmount                    */
   {
      if (strncmp(tape_device, "on-line", 7) != 0
          || strncmp(tape_device, "ON-LINE", 7) != 0)
      {
         tapefd = open(tape_device, O_RDONLY | O_NONBLOCK);
         if (tapefd == -1)
         {
            switch (errno)
            {
            case ENOENT:
               *ierr = 2;
               break;
            default:
               *ierr = 6;
            }
            *syserr = errno;
            sprintf(msg, "ZMOUN2: Could not open tape device %s",
                    tape_device);
         }

         if (*ierr == 0)
         {
            /*
             * Take tape offline:
             */

            tape_operation.mt_count = 1;
            tape_operation.mt_op    = MTUNLOAD;
            status = ioctl(tapefd, MTIOCTOP, &tape_operation);
            if (status != 0)
            {
               *ierr = 3;
               *syserr = errno;
               sprintf(msg, "ZMOUN2: error %d taking drive offline",
                       errno);
            }
         }

         close(tapefd);
         status = close(lockfd);
         unlink(lockfile);
      }
   }
}
