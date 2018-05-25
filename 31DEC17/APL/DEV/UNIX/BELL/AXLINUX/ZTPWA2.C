#define Z_tpwa2__
                                        /* Must not claim POSIX_SOURCE*/
                                        /* types.h would not define   */
                                        /* things needed in mtio.h    */
#ifdef _POSIX_SOURCE
#undef _POSIX_SOURCE
#endif

#include <unistd.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

#define MT_Debug 0                      /* debug message control      */

#if __STDC__
   void ztpwa2_(int *fcb, int *ierr)
#else
   ztpwa2_(fcb, ierr)
   int *fcb, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! wait for read/write from/to a tape device                         */
/*# IO-basic Tape                                                     */
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
/*  Wait for the completion of an asynchronous tape I/O operation and */
/*  returns its completion status and 8-bit byte count.               */
/*  Inputs:                                                           */
/*     fcb    I(*)   File control block for opened tape drive & buffer*/
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => no error                 */
/*                      3 => I/O error                                */
/*                      4 => end of file                              */
/*                      5 => beginning of tape                        */
/*                      6 => end of information                       */
/*  Linux version - only synchronous I/O, read status however         */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   struct mtget stat;
   int fd;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* If non-zero byte request,  */
                                        /* wait for I/O to complete   */
                                        /* and check status.          */
   if (*(fcb + Z_fcbreq) != 0) {
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZTPOP2.    */
      fd = *(fcb + Z_fcbfd);
                                        /* Call to wait function goes */
                                        /* here.                      */

                                        /* Error?                     */
      if (*(fcb + Z_fcbxfr) == -1) {
         *ierr = 3;
         }
                                        /* End of file?               */
      else if (*(fcb + Z_fcbxfr) == 0) {
         *ierr = 4;
         }
      if (fd < 0) return ;
                                        /* get status                 */
      if (*ierr != 0) {
         if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTPWA2: error getting drive status\n");
            perror("ZTPWA2");
            }
         else {
            if (GMT_BOT(stat.mt_gstat))      *ierr = 5;
            else if (GMT_EOF(stat.mt_gstat)) *ierr = 4;
            else if (GMT_EOT(stat.mt_gstat)) *ierr = 6;
            else if (GMT_EOD(stat.mt_gstat)) *ierr = 6;
                                        /* This is unreliable!!       */
            if ((*(fcb + Z_fcberr) == 5) && (*ierr == 3)) *ierr = 6;
#if MT_Debug                            /*  NOTE - debug              */
            printf("ZTPWA2: errno = %d \n", *(fcb + Z_fcberr));
            printf("ZTPWA2: stat.mt_gstat, erreg (hex) = %x , %x\n",
               stat.mt_gstat,stat.mt_erreg);
            printf("ZTPWA2: stat.mt_type=%d, out resid=%d, out ierr=%d\n",
               stat.mt_type,stat.mt_resid,*ierr);
            printf("ZTPWA2: stat.mt_dsreg = %x\n", stat.mt_dsreg);
            printf("ZTPWA2: stat.mt_fileno mt_blkno = %d %d\n",
               stat.mt_fileno,stat.mt_blkno);
#endif
            }
         }
      }

   return;
}
