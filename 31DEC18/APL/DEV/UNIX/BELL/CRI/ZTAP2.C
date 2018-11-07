#include <sys/types.h>
#include <sys/bmxctl.h>
#include <tapereq.h>
/*
#include <sys/ioctl.h>
#include <sys/mtio.h>
*/
#include <errno.h>

ztap2_(oper, count, fcb, ierr)
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
/*                 'BEGW' = mark to begin write                       */
/*                 'AEOI' = advance to end-of-medium                  */
/*  In/out:                                                           */
/*     fcb   I(*)  File descriptor from FTAB file control block       */
/*     count I     Number of times to do operation                    */
/*                 Out: AEOI only - EOI after COUNT-1 files if > 0    */
/*                       EOI after forward skip -COUNT-1 files if < 0 */
/*  Output:                                                           */
/*     ierr  I     Error code: 0 = success                            */
/*                    2 = input specification error                   */
/*                    3 = i/o error                                   */
/*                    4 = tape mark encountered                       */
/*                    5 = tape at load point                          */
/*                    6 = tape at physical end                        */
/*  UNICOS version: does not work - partly stubbed                    */
/*--------------------------------------------------------------------*/
char oper[4];
int *fcb, *count, *ierr;
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # FTAB elements per map    */
                                        /* file control block         */
   extern int Z_mfcb;
   int fd, n7 = 7;
   register int i;
   char op[5], msg[80];
   void zmsgwr_();
/*
   struct mtop top;
   struct mtget stat;
*/
   struct dmn_comm pos;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   sprintf (msg, "UniCos ZTAP2 is very incomplete - use FITS disk");
   zmsgwr_ (msg, &n7);
                                        /* Form null terminated opcode*/
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Zero other I/O status      */
                                        /* entries in the file        */
                                        /* control block.             */
   for (i = 2; i < Z_mfcb; i++)
      *(fcb + i) = 0;
                                        /* Set count in structure.    */
/*
   top.mt_count = 1;
*/
   pos.POS_COUNT = 1;
                                        /* Issue null operation.      */
                                        /* MTIOCTOP always seems to   */
                                        /* yield an error (ignore it).*/
   fd = *(fcb + Z_fcbfd);
/*
   top.mt_op = MTNOP;
   ioctl (fd, MTIOCTOP, &top);
*/
/*
   top.mt_count = *count;
*/
                                        /* Get drive status.          */
/*
   if (ioctl (fd, MTIOCGET, &stat) == -1) {
      *(fcb + Z_fcberr) = errno;
      *ierr = 3;
      }
*/
                                        /* Rewind?                    */
   if (strcmp (op, "REWI") == 0) {
      pos.POS_REQ = TR_RWD;
      pos.POS_COUNT = 1;
      }
                                        /* Forward space file?        */
   else if (strcmp (op, "ADVF") == 0) {
      pos.POS_REQ = TR_PTMS;
      pos.POS_COUNT = *count;
      }
                                        /* Back space file?           */
   else if (strcmp (op, "BAKF") == 0) {
      pos.POS_REQ = TR_PTMS;
      pos.POS_COUNT = -(*count);
      }
                                        /* Forward space record?      */
   else if (strcmp (op, "ADVR") == 0) {
      pos.POS_REQ = TR_PBLKS;
      pos.POS_COUNT = *count;
      }
                                        /* Back space record?         */
   else if (strcmp (op, "BAKR") == 0) {
      pos.POS_REQ = TR_PBLKS;
      pos.POS_COUNT = -(*count);
      }
                                        /* Write EOF: not implemented */
                                        /* BEGW, AEOI also missing    */
/*
   else if ((strcmp(op,"WEOF") == 0) || (strcmp(op,"BEGW") == 0)) {
      pos.POS_REQ = TR_???;
      pos.POS_COUNT = -1;
      }
   else if ((strcmp(op,"EOM ") == 0) || (strcmp(op,"AEOI") == 0)) {
      }
*/
                                        /* Input error.               */
   else
      *ierr = 2;
                                        /* Order up operation.        */
   if (*ierr == 0) {
                                        /* MTIOCTOP always seems to   */
                                        /* yield an error (ignore it).*/
/*
      ioctl (fd, MTIOCTOP, &top);
*/
      if (ioctl (fd, BXC_SPOS, &pos) == -1) {
         *(fcb + Z_fcberr) = errno;
         *ierr = 3;
         }
      else if (ioctl (fd, BXC_GPOS, &pos) == -1) {
         *(fcb + Z_fcberr) = errno;
         *ierr = 3;
         }
                                        /* Where are we?              */
      else {
         if (pos.POS_REP != 0)
            printf ("ZTAP2: pos.POS_REP = %d\n", pos.POS_REP);
                                        /* Beginning of tape?         */
/*
         if ((stat.mt_dsreg & MT_BOT) != 0)
            *ierr = 5;
*/
                                        /* End of tape?               */
/*
         else if ((stat.mt_dsreg & MT_EOT) != 0)
            *ierr = 6;
*/
                                        /* Tape mark?                 */
/*
         else if ((stat.mt_dsreg & MT_TM) != 0)
            *ierr = 4;
*/
         }
      }

   return;
}
