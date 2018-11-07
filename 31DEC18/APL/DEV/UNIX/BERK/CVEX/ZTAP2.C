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
/*  Convex version                                                    */
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
   int fd, wasatbot, jc, n3 = 3, n7 = 7;
   register int i;
   char op[5], msg[80];
   struct mtop top;
   struct mtget stat;
/*--------------------------------------------------------------------*/
   *ierr = 0;
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
   top.mt_count = 1;
                                        /* Issue null operation.      */
                                        /* MTIOCTOP always seems to   */
                                        /* yield an error (ignore it).*/
   top.mt_op = MTNOP;
   fd = *(fcb + Z_fcbfd);
   ioctl (fd, MTIOCTOP, &top);
                                        /* Get drive status.          */
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
   else if ((strcmp (op, "WEOF") == 0) || (strcmp (op, "BEGW") == 0)) {
      wasatbot = 0;
      if (strcmp (op, "BEGW") == 0) {
         top.mt_op = MTBSF;
         ioctl (*fcb, MTIOCTOP, &top);
         if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTAP2: error getting drive status\n");
            perror("ZTAP2");
            }
         else {
/*                                        NOTE - debug
            printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
               "BEGW BSF1",top.mt_op,*ierr);
            printf("ZTAP2: stat.mt_type = %d\n",
               stat.mt_type);
            printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
               stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
            printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
               stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
            printf("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
               stat.mt_fileno,stat.mt_blkno);
            printf("ZTAP2 wasatbot = %d\n", wasatbot);
*/
/*            if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
               wasatbot = 1;   does not always work */
            if ((stat.mt_dsreg & MT_BOT) != 0)  wasatbot = 1;
            }
         }
                                          /* 2 EOFs 1/2 inch */
      if (*ierr == 0) {
         top.mt_op = MTWEOF;
         top.mt_count = 1;
         if (stat.mt_type == MT_ISTA) top.mt_count = 2;
         ioctl (*fcb, MTIOCTOP, &top);
         if (ioctl (*fcb, MTIOCGET, &stat) != 0) {
            *ierr = 3;
            printf("ZTAP2: error getting drive status\n");
            perror("ZTAP2");
            }
         else {
/*                                        NOTE - debug
            printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
               op,top.mt_op,*ierr);
            printf("ZTAP2: stat.mt_type = %d\n",
               stat.mt_type);
            printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
               stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
            printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
               stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
            printf("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
               stat.mt_fileno,stat.mt_blkno);
            printf("ZTAP2 wasatbot = %d\n", wasatbot);
*/
            top.mt_op = MTBSF;
             ioctl (*fcb, MTIOCTOP, &top);
            if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
               *ierr = 3;
               printf("ZTAP2: error getting drive status\n");
               perror("ZTAP2");
               }
            else {
/*                                        NOTE - debug
               printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                  "BSF(2)",top.mt_op,*ierr);
               printf("ZTAP2: stat.mt_type = %d\n",
                  stat.mt_type);
               printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
                  stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
               printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
                  stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
               printf("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
                  stat.mt_fileno,stat.mt_blkno);
               printf("ZTAP2 wasatbot = %d\n", wasatbot);
*/
               top.mt_op = MTFSF;
               top.mt_count = 1;
               if (wasatbot) top.mt_op = MTNOP;
/*             if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
                  top.mt_op = MTNOP;             does not always work */
               if ((stat.mt_dsreg & MT_BOT) != 0) top.mt_op = MTNOP;
               }
            }
         }
      }
                                     /* go to EOM, AEOI               */
   else if ((strcmp(op,"EOM ") == 0) || (strcmp(op,"AEOI") == 0)) {
      for (jc = 0; jc < 10000; jc++) {
         top.mt_op = MTFSR;
         ioctl (fd, MTIOCTOP, &top);
         if (ioctl (*fcb, MTIOCGET, &stat) != 0) {
            *ierr = 3;
            *(fcb + Z_fcberr) = errno;
            break;
            }
                                        /* Where are we?              */
         else {
                                        /* End of tape?               */
            if ((stat.mt_dsreg & MT_EOT) != 0)
               *ierr = 6;
                                        /* Tape mark?                 */
            if ((stat.mt_dsreg & MT_TM) != 0)
               *ierr = 4;
            }
         if (*ierr != 0) {
            *ierr = 6;
            sprintf (msg,
            "Advanced to end-of-information after skipping %d files\0",
               jc); 
            zmsgwr_ (msg, &n3);
            break;
            }
         else {
            top.mt_op = MTFSF;
            ioctl (fd, MTIOCTOP, &top);
            if (ioctl (*fcb, MTIOCGET, &stat) != 0) {
               *ierr = 3;
               *(fcb + Z_fcberr) = errno;
               break;
               }
                                        /* End of tape?               */
            else {
               if ((stat.mt_dsreg & MT_EOT) != 0) *ierr = 6;
               }
            if (*ierr != 0) {
               *ierr = 6;
               sprintf (msg,
            "Advanced to end-of-information after skipping %d files\0",
                  jc); 
               zmsgwr_ (msg, &n3);
               break;
               }
            }
         }
      if (jc >= 10000) {
         *ierr = 3;
         sprintf (msg, "ZTAP2 advance 10000 files with no EOI");
         zmsgwr_ (msg, &n7);
         }
                                        /* Reposition by one          */
      else if (*ierr == 6) {
         top.mt_op = MTBSR;
         ioctl (fd, MTIOCTOP, &top);
         if (ioctl (*fcb, MTIOCGET, &stat) != 0) {
            *ierr = 3;
            *(fcb + Z_fcberr) = errno;
            }
         else {
/*                                        NOTE - debug
            printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
               "BSF(2)",top.mt_op,*ierr);
            printf("ZTAP2: stat.mt_type = %d\n",
               stat.mt_type);
            printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
               stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
            printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
               stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
            printf("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
               stat.mt_fileno,stat.mt_blkno);
            printf("ZTAP2 wasatbot = %d\n", wasatbot);
*/
            }
         }
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
         *(fcb + Z_fcberr) = errno;
         *ierr = 3;
         }
                                        /* Where are we?              */
      else {
                                        /* Beginning of tape?         */
         if ((stat.mt_dsreg & MT_BOT) != 0)
            *ierr = 5;
                                        /* End of tape?               */
         else if ((stat.mt_dsreg & MT_EOT) != 0)
            *ierr = 6;
                                        /* Tape mark?                 */
         else if ((stat.mt_dsreg & MT_TM) != 0)
            *ierr = 4;
/*                                        NOTE - debug
         printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
            op,top.mt_op,*ierr);
         printf("ZTAP2: stat.mt_type = %d\n",
            stat.mt_type);
         printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
            stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
         printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
            stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
         printf("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
            stat.mt_fileno,stat.mt_blkno);
         printf("ZTAP2 wasatbot = %d\n", wasatbot);
*/
         }
      }
                                        /* At the moment, always      */
                                        /* wait.  Otherwise, the byte */
                                        /* count will get screwed up  */
                                        /* for actual transfers.      */
                                        /* (i.e., tape manipulations  */
                                        /* get counted as a 512 byte  */
                                        /* transfers for some screwy  */
                                        /* reason).  Wait on both     */
                                        /* file descriptors since you */
                                        /* never know where Convex is */
                                        /* going to put their fucked  */
                                        /* up byte count.             */
   asiostat (fd);
   fd = *(fcb + Z_mfcb);
   asiostat (fd);

   return;
}
