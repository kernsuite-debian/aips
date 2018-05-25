#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

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

ztap2_(opr, fcb, ierr)
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
/*   VAX UNIX version - 4.2 bsd (grossly system/device dependent).    */
/*--------------------------------------------------------------------*/
int *fcb, *ierr;
char opr[4];
/*--------------------------------------------------------------------*/
{
   struct mtop top;
   struct mtget stat;
   char op[5];
   int i;
/*--------------------------------------------------------------------*/
   for (i = 0; i < 4; i++)
      op[i] = opr[i];
   op[4] = '\0';
   *ierr = 0;
   if (strcmp(op,"MONT") == 0)
      return;
   else if (strcmp(op,"REWI") == 0)
      top.mt_op = MTREW;
   else if (strcmp(op,"ADVF") == 0)
      top.mt_op = MTFSF;
   else if (strcmp(op,"BAKF") == 0)
      top.mt_op = MTBSF;
   else if (strcmp(op,"ADVR") == 0)
      top.mt_op = MTFSR;
   else if (strcmp(op,"BAKR") == 0)
      top.mt_op = MTBSR;
   else if (strcmp(op,"WEOF") == 0)
      top.mt_op = MTWEOF;
   else if (strcmp(op,"DMNT") == 0)
      top.mt_op = MTOFFL;
   else {
      *ierr = 3;
      return;
      }
   top.mt_count = 1;
   i = ioctl (*fcb, MTIOCTOP, &top);
   if (i != 0) *ierr = 3;
   top.mt_op = MTNOP;
   i = ioctl (*fcb, MTIOCTOP, &top);
   i = ioctl (*fcb, MTIOCGET, &stat);
/*                                             HT drives     */
   if (stat.mt_type == 2) {
      if ((stat.mt_dsreg & ERR) != 0) {
         *ierr = 3;
         if ((stat.mt_erreg & (NERR1 | NERR2)) != 0) goto marker;
         if ((stat.mt_erreg & (NERR3 | NERR4)) != 0) {
            if ((stat.mt_dsreg & BOT) != 0) *ierr = 5;
            }
         goto endit;
         }
marker:  ;
      *ierr = 0;
      if ((stat.mt_dsreg & BOT) != 0) *ierr = 5;
      else if ((stat.mt_dsreg & EOT) != 0) *ierr = 6;
      else if ((stat.mt_dsreg & TM ) != 0) *ierr = 4;
      goto endit;
      }
/*                                           TM drives      */
   else {
      if ((stat.mt_dsreg & ERR3) != 0) *ierr = 3;
      if ((stat.mt_erreg & BOT3) != 0) *ierr = 5;
      else if ((stat.mt_erreg & EOT3) != 0) *ierr = 6;
      else if ((stat.mt_erreg & TM3 ) != 0) *ierr = 4;
      }
endit: ;
   *(fcb + 2) = *ierr;

   return;
}
