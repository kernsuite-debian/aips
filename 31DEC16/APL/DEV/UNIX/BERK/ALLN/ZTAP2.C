#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <errno.h>

ztap2_(oper, fcb, ierr)
/*--------------------------------------------------------------------*/
/*! tape movements as called by ZTAPE                                 */
/*# Z2 Tape                                                           */
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
/*  Performs standard tape manipulating functions.                    */
/*  Inputs:                                                           */
/*     opr   C*4   Operation to be performed:                         */
/*                 'REWI' = rewind tape                               */
/*                 'ADVF' = advance file                              */
/*                 'BAKF' = backspace file                            */
/*                 'ADVR' = advance record                            */
/*                 'BAKR' = backspace record                          */
/*                 'WEOF' = write end of file                         */
/*  In/out:                                                           */
/*     fcb   I(*)  File descriptor from FTAB file control block       */
/*  Output:                                                           */
/*     ierr  I     Error code: 0 = success                            */
/*                    2 = input specification error                   */
/*                    3 = i/o error                                   */
/*                    4 = tape mark encountered                       */
/*                    5 = tape at load point                          */
/*                    6 = tape at physical end                        */
/* Alliant version (uses virtual status registers).                   */
/*--------------------------------------------------------------------*/
char oper[4];
int *fcb, *ierr;
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # FTAB elements per map    */
                                        /* file control block         */
   extern int Z_mfcb;
   extern int errno;
   int fd;
   register int i;
   char op[5];
   struct mtop top;
   struct mtvsr vstat;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Form null terminated       */
                                        /* operation code.            */
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Zero other I/O status      */
                                        /* entries in the file        */
                                        /* control block.             */
   for (i = 2; i < Z_mfcb; i++)
      *(fcb + i) = 0;
                                        /* Set count in structure.    */
   top.mt_count = 1;
                                        /* Issue null operation.      */
   top.mt_op = MTNOP;
   fd = *(fcb + Z_fcbfd);
   ioctl (fd, MTIOCTOP, &top);
                                        /* Get drive status.          */
   if (ioctl (fd, MTIOCVSR, &vstat) == -1) {
      *(fcb + Z_fcberr) = errno;
      *ierr = 3;
      }
                                        /* Rewind?                    */
   else if (strcmp (op, "REWI") == 0)
      top.mt_op = MTREW;
                                        /* Forward space file?        */
   else if (strcmp (op, "ADVF") == 0)
      top.mt_op = MTFSF;
                                        /* Back space file?           */
   else if (strcmp (op, "BAKF") == 0)
      top.mt_op = MTBSF;
                                        /* Forward space record?      */
   else if (strcmp (op, "ADVR") == 0)
      top.mt_op = MTFSR;
                                        /* Back space record?         */
   else if (strcmp (op, "BAKR") == 0)
      top.mt_op = MTBSR;
                                        /* Write EOF?                 */
   else if (strcmp (op, "WEOF") == 0)
      top.mt_op = MTWEOF;
                                        /* Input error.               */
   else
      *ierr = 2;
                                        /* Order up operation.        */
   if (*ierr == 0) {
      ioctl (fd, MTIOCTOP, &top);
      if (ioctl (fd, MTIOCVSR, &vstat) == -1) {
         *(fcb + Z_fcberr) = errno;
         *ierr = 3;
         }
                                        /* Where are we?              */
      else {
                                        /* Beginning of tape?         */
         if ((vstat.mt_status & MTVSR_BOT) != 0)
            *ierr = 5;
                                        /* End of tape?               */
         else if ((vstat.mt_status & MTVSR_EOT) != 0)
            *ierr = 6;
                                        /* Tape mark?                 */
         else if ((vstat.mt_status & MTVSR_EOF) != 0)
            *ierr = 4;
         }
      }

   return;
}
