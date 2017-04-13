#include <sys/types.h>
#include <sys/tape.h>
#include <sys/ioctl.h>
/* #include <sys/mtio.h>  doesn't exist on the IBM*/
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
/*     count I     In: Number of files/records to skip                */
/*                 Out: on AEOI, file # at EOI, < 0 => relative       */
/*     fcb   I(*)  File descriptor from FTAB file control block       */
/*  Output:                                                           */
/*     ierr  I     Error code: 0 = success                            */
/*                    2 = input specification error                   */
/*                    3 = i/o error                                   */
/*                    4 = tape mark encountered                       */
/*                    5 = tape at load point                          */
/*                    6 = tape at physical end                        */
/*  IBM RISC 6000 version                                             */
/*--------------------------------------------------------------------*/
char oper[4];
int *count, *fcb, *ierr;
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
   int fd, ierr2, n7 = 7;
   register int i, jc;
   char op[5], msg[80];
   struct stop top;
/*  struct mtget stat;  doesn't exist on the IBM */
   extern int IBM_taptyp;      /* 0-3 unknown,reel,Exabyte,DAT        */
/*--------------------------------------------------------------------*/
   *ierr = 0;
   ierr2 = 0;
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
   top.st_count = *count;
   fd = *(fcb + Z_fcbfd);
                                        /* Issue null operation.      */
                                        /* MTIOCTOP always seems to   */
                                        /* yield an error (ignore it).*/
 /* There is no equivalent for the following in AIX
  *  top.st_op = MTNOP;
  *  ioctl (fd, MTIOCTOP, &top);
  *                                        * Get drive status.        *
  *  if (ioctl (fd, MTIOCTOP, &stat) == -1) {
  *     *(fcb + Z_fcberr) = errno;
  *     *ierr = 3;
  *     }
  */
                                        /* Rewind?                    */
   if (strcmp (op, "REWI") == 0) {
      top.st_count = 1;
      ierr2 = 5;                        /* Set BOT                    */
      top.st_op = STREW;
    }
                                        /* Forward space file?        */
   else if (strcmp (op, "ADVF") == 0) {
      ierr2 = 4;                        /* Set TM                     */
      top.st_op = STFSF;
    }
                                        /* Back space file?           */
   else if (strcmp (op, "BAKF") == 0) {
      ierr2 = 4;                        /* Set TM                     */
      top.st_op = STRSF;
    }
                                        /* Forward space record?      */
   else if (strcmp (op, "ADVR") == 0)
      top.st_op = STFSR;
                                        /* Back space record?         */
   else if (strcmp (op, "BAKR") == 0)
      top.st_op = STRSR;
                                        /* Write EOF?                 */
   else if ((strcmp (op, "WEOF") == 0) || (strcmp(op,"BEGW") == 0)) {
      top.st_count = 1;
      ierr2 = 4;                        /* Set TM                     */
      if (strcmp (op,"BEGW") == 0) {
         top.st_op = STRSF;
         if (ioctl (fd, STIOCTOP, &top) == -1) {
            *ierr = 5;
/*          printf ("ZTAP2 DEBUG: at bot on BEGWs 1st RSF\n");       */
            }
         }
      if (*ierr == 0) {
         top.st_op = STWEOF;
         top.st_count = 1;
         if (IBM_taptyp < 2) top.st_count += 1;
         if (ioctl (fd, STIOCTOP, &top) == -1) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 3;
            sprintf (msg, "ZTAP2 at %s get error doing WEOF",op);
            zmsgwr_ (msg, &n7);
            }
         else {
            top.st_op = STRSF;
            top.st_count -= 1;
            *ierr = 4;
            if (top.st_count) {
               if (ioctl (fd, STIOCTOP, &top) == -1) {
                  *ierr = 5;
/*                printf ("ZTAP2 DEBUG: at bot on BEGWs 2nd RSF\n"); */
                  }
               }
            }
         }
      }
                                  /* Advance to end-of-information   */
   else if (strcmp (op, "AEOI") == 0) {
      top.st_count = 1;
      for (jc = 0; jc < 40000; jc++) {
         top.st_op = STFSR;
         if (ioctl (fd, STIOCTOP, &top) == -1) {
            *ierr = 6;
            *count = -jc -1;
            break;
            }
         else {
            top.st_op = STFSF;
            if (ioctl (fd, STIOCTOP, &top) == -1) {
               *ierr = 6;
               *count = -jc -1;
               break;
               }
            }
         }
      if (jc >= 40000) {
         *ierr = 3;
         sprintf (msg, "ZTAP2 advance 40000 files with no EOI");
         zmsgwr_ (msg, &n7);
         }
      }
                                        /* Input error.               */
   else
      *ierr = 2;
                                        /* Order up operation.        */
   if (*ierr == 0) {
      if (ioctl (fd, STIOCTOP, &top) == -1) {
         *(fcb + Z_fcberr) = errno;
         switch ((int) top.st_op) {
         case STREW:
            *ierr = 3;
            break;
         case STWEOF:
            *ierr = 3;
            break;
                                        /* Fails => EOT?            */
         case STFSF:
            *ierr = 6;
            break;
                                        /* Fails => BOT             */
         case STRSF:
            *ierr = 5;
            break;
                                        /* Move past file mark       */
         case STFSR:
            top.st_op = STFSF;
                                        /* fails => End of Tape      */
            if (ioctl (fd, STIOCTOP, &top) == -1) *ierr = 6;
                                        /* else return TM            */
            else *ierr = 4;
            break;
                                        /* Move past file mark       */
         case STRSR:
            top.st_op = STRSF;
                                        /* fails => Beg. of Tape     */
            if (ioctl (fd, STIOCTOP, &top) == -1) *ierr = 5;
                                        /* else return TM            */
            else *ierr = 4;

            break;
         default:
            *ierr = 3;
            }
         }
                                        /* Return expected "error"   */
      else {
                                        /* advance over last EOF   */
         if ((top.st_op == STRSF) && (*ierr == 0)) {
            top.st_op = STFSF;
            top.st_count = 1;
            if (ioctl (fd, STIOCTOP, &top) == -1) {
               *(fcb + Z_fcberr) = errno;
               *ierr = 3;
               }
            }
         if (*ierr == 0) *ierr = ierr2;
         }
      }
exit:
   return;
}
