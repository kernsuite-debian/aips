#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

/* Version for Sun Xylogics xt and SCSI st drivers. */

/* From <sundev/scsi.h> : */
#define SC_BOT    0x15            /* driver, bot hit */
#define SC_EOT    0x13            /* driver, eot hit */
#define SC_EOF    0x12            /* driver, eof hit */
#define SC_BLCHK  0x08            /* blank => eot ?  */

/* From <sundev/xtreg.h> : */
#define XTS_EOT   0x80            /* end of tape status */
#define XTE_EOT   0x31            /* end of tape error */
#define XTS_BOT   0x40            /* beginning of tape status */
#define XTE_BOT   0x30            /* beginning of tape error */
#define XTE_EOF   0x1E            /* end of file error */

ztpwa2_(fcb, ierr)
/*--------------------------------------------------------------------*/
/*! wait for read/write from/to a tape device                         */
/*# IO-basic Tape                                                     */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
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
/*  SUN version - only synchronous I/O, read status however.          */
/*--------------------------------------------------------------------*/
int *fcb, *ierr;
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   extern int errno;
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
      if (*ierr != 0) {
                                        /* get status                 */
         if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTPWA2: error getting drive status\n");
            perror("ZTPWA2");
            }
         else {
            if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
               *ierr = 5; 
            else if (stat.mt_type == MT_ISXY) {
               if ((stat.mt_dsreg & XTS_BOT) != 0)
                  *ierr = 5;
               else if ((stat.mt_dsreg & XTS_EOT) != 0)
                  *ierr = 6;
               else if (stat.mt_erreg == XTE_BOT) 
                  *ierr = 5;
               else if (stat.mt_erreg == XTE_EOF)
                  *ierr = 4;
               else if (stat.mt_erreg == XTE_EOT)
                  *ierr = 6;
               }
            else {                      /* SCSI tape.  */
               if (stat.mt_erreg == SC_BOT) 
                  *ierr = 5;
               else if (stat.mt_erreg == SC_EOF)
                  *ierr = 4;
               else if (stat.mt_erreg == SC_EOT)
                  *ierr = 6;
               else if (stat.mt_erreg == SC_BLCHK)
                  *ierr = 6;
               }
/*                                           NOTE - debug
            printf("ZTPWA2: stat.mt_dsreg = %o %d %x\n",
               stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
            printf("ZTPWA2: stat.mt_erreg = %o %d %x\n",
               stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
            printf("ZTPWA2: stat.mt_filno = %d stat.mt_blkno = %d\n",
               stat.mt_fileno,stat.mt_blkno);
            printf("ZTPWA2: stat.mt_type = %d, out ierr = %d\n",
               stat.mt_type,*ierr);
*/
            }
         }
      }

   return;
}
