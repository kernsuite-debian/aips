#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <errno.h>

#define BBOT3   011      /* Before load point */
#define BOT3    020000   /* Load point */
#define TM3     0100000  /* Tape mark */
#define EOT3    010000   /* Physical end of tape */
#define FILPRT  0200     /* Write protected */

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
/*     count I     In : # records/files to skip                       */
/*                 Out: AEOI only - EOI after COUNT-1 files if > 0    */
/*                       EOI after forward skip -COUNT-1 files if < 0 */
/*  Output:                                                           */
/*     ierr  I     Error code: 0 = success                            */
/*                    2 = input specification error                   */
/*                    3 = i/o error                                   */
/*                    4 = tape mark encountered                       */
/*                    5 = tape at load point                          */
/*                    6 = tape at physical end                        */
/*  Masscomp version (TM drive). NOT TESTED AFTER REVISION 5/11/93    */
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
   extern int errno;
   int fd, wasatbot, nr, status;
   register int i;
   char op[5], record[32768];
   struct mtop top;
   struct mtget stat;
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
                                        /* MTIOCTOP always seems to   */
                                        /* yield an error (ignore it).*/
   top.mt_op = MTNOP;
   fd = *(fcb + Z_fcbfd);
   ioctl (fd, MTIOCTOP, &top);
                                        /* Get drive status.          */
   if (*count <= 0) *count = 1;
   top.mt_count = *count;
   if (ioctl (fd, MTIOCGET, &stat) == -1) {
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
                                        /* WEOF and BEGW             */
   else if ((strcmp(op,"WEOF") == 0) || (strcmp(op,"BEGW") == 0)) {
      wasatbot = 0;
      if (strcmp(op,"BEGW") == 0) {
         top.mt_op = MTBSF;
         top.mt_count = 1;
         status = ioctl(*fcb,MTIOCTOP,&top);
         if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTAP2: error getting drive status\n");
            perror("ZTAP2");
            }
         else {
                                        /* Beginning of tape?         */
            if ((stat.mt_erreg & BOT3) != 0) {
               wasatbot = 1;
                                        /* Before beginning of tape?  */
               if ((stat.mt_erreg & BBOT3) != 0) {
                  close (*fcb);
                  if ((stat.mt_erreg & FILPRT) != 0)
                     *fcb = open ("/dev/nr1600mt0", 0);
                  else
                     *fcb = open ("/dev/nr1600mt0", 2);
                  }
               }
            }
         }
      if (*ierr == 0) {
         top.mt_op = MTWEOF;
         top.mt_count = 2;
         status = ioctl(*fcb,MTIOCTOP,&top);
         if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTAP2: error getting drive status\n");
            perror("ZTAP2");
            }
         else {
            top.mt_op = MTBSF;
            if (wasatbot) top.mt_op = MTREW;
            status = ioctl (*fcb, MTIOCTOP, &top);
            if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
               *ierr = 3;
               printf("ZTAP2: error getting drive status\n");
               perror("ZTAP2");
               }
            else {
               top.mt_count = 1;
               if (wasatbot) 
                  top.mt_op = MTNOP;
               else
                  top.mt_op = MTFSF;
                                     /* Beginning of tape?         */
               if ((stat.mt_erreg & BOT3) != 0)
                  top.mt_op = MTNOP;
               }
            }
         }
      }
                                        /* FNDEOT in C             */
   else if ((strcmp(op,"EOM ") == 0) || (strcmp(op,"AEOI") == 0)) {
      for (i = 0; i < 10000; i++) {
         nr = read (*fcb, record, 32768);
         if (nr <= 0) {
            if (nr < 0) *ierr = 3;
            break;
            }
         else {
            top.mt_op = MTFSF;
            top.mt_count = 1;
            status = ioctl(*fcb,MTIOCTOP,&top);
            }
         }
      *count = -i - 1;
      if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
         *ierr = 3;
         printf("ZTAP2: error getting drive status\n");
         perror("ZTAP2");
         }
      else {
                                        /* Beginning of tape?         */
         if ((stat.mt_erreg & BOT3) != 0) {
            *ierr = 5;
                                        /* Before beginning of tape?  */
            if ((stat.mt_erreg & BBOT3) != 0) {
               close (*fcb);
               if ((stat.mt_erreg & FILPRT) != 0)
                  *fcb = open ("/dev/nr1600mt0", 0);
               else
                  *fcb = open ("/dev/nr1600mt0", 2);
               }
            }
                                        /* End of tape?               */
         else if ((stat.mt_erreg & EOT3) != 0)
            *ierr = 6;
                                        /* Tape mark?                 */
         else if ((stat.mt_erreg & TM3 ) != 0)
            *ierr = 4;
         }
      top.mt_op = MTBSF;
      top.mt_count = 2;
      if (*ierr == 6) top.mt_count = 1;
      if ((*ierr == 6) || (*ierr == 4)) *ierr = 0;
      }

                                        /* Input error.               */
   else
      *ierr = 2;
                                        /* Order up operation.        */
   if (*ierr == 0) {
                                        /* MTIOCTOP always seems to   */
                                        /* yield an error (ignore it).*/
      ioctl (fd, MTIOCTOP, &top);
      if (ioctl (fd, MTIOCGET, &stat) == -1) {
         *ierr = 3;
         }
                                        /* Where are we?              */
      else {
                                        /* Beginning of tape?         */
         if ((stat.mt_erreg & BOT3) != 0) {
            *ierr = 5;
                                        /* Before beginning of tape?  */
            if ((stat.mt_erreg & BBOT3) != 0) {
               close (*fcb);
               if ((stat.mt_erreg & FILPRT) != 0)
                  *fcb = open ("/dev/nr1600mt0", 0);
               else
                  *fcb = open ("/dev/nr1600mt0", 2);
               }
            }
                                        /* End of tape?               */
         else if ((stat.mt_erreg & EOT3) != 0)
            *ierr = 6;
                                        /* Tape mark?                 */
         else if ((stat.mt_erreg & TM3 ) != 0)
            *ierr = 4;
         }
      }

   if (*ierr == 3)
      *(fcb + 2) = errno;
   else
      *(fcb + 2) = 0;

   return;
}
