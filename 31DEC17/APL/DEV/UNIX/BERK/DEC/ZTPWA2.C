#define Z_tpwa2__

#define MT_Debug 0                      /* debug message control      */

#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <sys/devio.h>

#define BOT 02
#define TM  04
#define EOT 02000
#define ERR 040000
#define NERR1 01000
#define NERR2 02000
#define NERR3 04000
#define NERR4 020000
#define ERR3  0100000
#define BOT3  040
#define TM3   040000
#define EOT3  02000

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
/*                      6 => end of tape                              */
/*  Generic UNIX version - only synchronous I/O.                      */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   struct mtget stat;
   struct devget devs;
   int fd, jerr;
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
      if (*ierr != 0) {
                                        /* get status                 */
         if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTPWA2: error getting drive status\n");
            perror("ZTPWA2");
            }
         else if (ioctl(*fcb,DEVIOCGET,&devs) != 0) {
            *ierr = 3;
            printf("ZTPWA2: error getting drive device status\n");
            perror("ZTPWA2");
            }
         else {
#if MT_Debug                              /* NOTE - debug            */
            if (errno != 0) perror ("ZTPWA2: ");
            printf("ZTPWA2: errno = %d  ierr = %d\n", errno,*ierr);
            printf("ZTPWA2: stat.mt_dsreg, erreg (hex) = %x , %x\n",
               stat.mt_dsreg,stat.mt_erreg);
            printf("ZTPWA2: stat.mt_type = %d, out resid = %d\n",
               stat.mt_type,stat.mt_resid);
               printf("ZTPWA2: devs.interf = %s, devs.dev = %s\n",
                  devs.interface, devs.device);
               printf("ZTPWA2: devs.stat = %x, devs.cat_stat = %x\n",
                  devs.stat, devs.category_stat);
#endif
            if (errno == ENOSPC)
               jerr = 6;
            else {
/*                                             HT drives     */
               jerr = 0;
               if (stat.mt_type == MT_ISHT) {
                  if ((stat.mt_dsreg & ERR) != 0) {
                     jerr = 3;
                     if ((stat.mt_erreg & (NERR1 | NERR2)) != 0) {
                        if ((stat.mt_dsreg & BOT) != 0) jerr = 5;
                        else if ((stat.mt_dsreg & EOT) != 0) jerr = 6;
                        else if ((stat.mt_dsreg & TM ) != 0) jerr = 4;
                        }
                     else if ((stat.mt_erreg & (NERR3 | NERR4)) != 0) {
                        if ((stat.mt_dsreg & BOT) != 0) jerr = 5;
                        }
                     }
                  }
/*                                           TM drives      */
               else {
                  if ((stat.mt_dsreg & ERR3) != 0) jerr = 3;
                  if ((stat.mt_erreg & BOT3) != 0) jerr = 5;
                  else if ((stat.mt_erreg & EOT3) != 0) jerr = 6;
                  else if ((stat.mt_erreg & TM3 ) != 0) jerr = 4;
                  }
               }
            if ((devs.stat & DEV_BOM) != 0) jerr = 5;
            if ((devs.stat & DEV_EOM) != 0) jerr = 6;
            if ((devs.category_stat & DEV_TPMARK) != 0) jerr = 4;
            if (jerr != 0) *ierr = jerr;
            }
         }
      }

   return;
}
