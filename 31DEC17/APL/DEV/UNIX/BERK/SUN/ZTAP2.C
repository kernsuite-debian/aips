#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <errno.h>

#define MT_Debug 0                      /* debug message control      */

/* Version for Sun Xylogics xt and SCSI st drivers. */
/* from <scsi/targets/stdef.h> */
#define	ST_TYPE_HIC		0x26	/* Generic 1/2" Cartridge */
#define	ST_TYPE_REEL		0x27	/* Generic 1/2" Reel Tape */

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

ztap2_(opr, count, fcb, ierr)
/*--------------------------------------------------------------------*/
/*! tape movements as called by ZTAPE                                 */
/*# Tape                                                              */
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
/*                                                                    */
/* Inputs:                                                            */
/*    opr    C*4   Operation to be performed:                         */
/*                 'REWI' = rewind tape                               */
/*                 'ADVF' = advance file                              */
/*                 'BAKF' = backspace file                            */
/*                 'ADVR' = advance record                            */
/*                 'BAKR' = backspace record                          */
/*                 'WEOF' = write end of file                         */
/*                 'BEGW' = mark to begin write                       */
/*                 'AEOI' = advance to end-of-medium                  */
/*  In/out:                                                           */
/*    fcb    I(*)  File descriptor from FTAB file control block       */
/*  Output:                                                           */
/*    ierr   I     Error code: 0 = success                            */
/*                    2 = input specification error                   */
/*                    3 = i/o error                                   */
/*                    4 = tape mark encountered                       */
/*                    5 = tape at load point                          */
/*                    6 = tape at physical end                        */
/*  SUN OS Exabyte & DAT version                                      */
/*  Modified to do nothing for real-time (fd < 0) "tapes"             */
/*--------------------------------------------------------------------*/
int *count, *fcb, *ierr;
char opr[4];
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd;
   extern int errno;
   struct mtop top;
   struct mtget stat;
   char op[5], record[32768], msg[80];
   int i, jc, nr, status, filenumb, n3 = 3, n7 = 7 ;
/*--------------------------------------------------------------------*/
   errno = 0;
   for(i = 0; i < 4; i++)
      op[i] = opr[i];
   op[4] = '\0';
   *ierr = 0;
                                        /* fake fd<0 ==> use Wes'     */
                                        /* on-line device.  No tape   */
                                        /* motions.                   */
   if (*(fcb + Z_fcbfd) < 0) return;

   top.mt_count = *count;

   top.mt_op = MTNOP;
   if (ioctl(*fcb,MTIOCTOP,&top) != 0) {
      *ierr = 3;
      printf("ZTAP2: error performing null op on tape drive\n");
      perror("ZTAP2");
      }
   else if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
      *ierr = 3;
      printf("ZTAP2: error getting drive status\n");
      perror("ZTAP2");
      }
   else {
#if MT_Debug                              /* NOTE - debug            */
      printf("ZTAP2 at NOP-0: op = %s top.mt_op = %d ierr = %d\n",
         op,top.mt_op,*ierr);
      printf("ZTAP2: stat.mt_type = %d\n",
         stat.mt_type);
      printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
         stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
      printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
         stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
#endif

      if (strcmp(op,"REWI") == 0)
         top.mt_op = MTREW;
      else if (strcmp(op,"ADVF") == 0)
         top.mt_op = MTFSF;
                                        /* leaves out cartridge tap*/
                                        /* ok 1/2inch, DAT, Exabyte*/
      else if (strcmp(op,"BAKF") == 0)
         top.mt_op = MTBSF;
                                        /* Only way to detect tape */
                                        /* marks.                  */
      else if (strcmp(op,"ADVR") == 0) {
         top.mt_count = 1;
         for (i=0; i < *count; i++) {
            nr = read (*fcb, record, 32768);
            if (nr == 0) {
               *ierr = 4;
               if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
                  printf("ZTAP2: error getting drive status\n");
                  perror("ZTAP2");
                  }
               else {
#if MT_Debug                              /* NOTE - debug            */
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
#endif
                  }
               if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
                  *ierr = 5;
               else if (stat.mt_type == MT_ISXY) {
                  if ((stat.mt_dsreg & XTS_BOT) != 0)
                     *ierr = 5;
                  else if ((stat.mt_dsreg & XTS_EOT) != 0)
                     *ierr = 6;
                  if (stat.mt_erreg == XTE_BOT)
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
               i = *count;
               }
            }
         top.mt_op = MTNOP;
         }
                                        /* Only way to detect tape */
                                        /* marks.  UGLYYY          */
      else if (strcmp(op,"BAKR") == 0) {
         top.mt_count = 1;
         for (i=0; i < *count; i++) {
            top.mt_op = MTBSR;
            if (ioctl(*fcb,MTIOCTOP,&top) != 0)
               *ierr = 3;
            else if ((nr = read(*fcb,record,32768)) == 0)
               *ierr = 4;
            else if (ioctl(*fcb,MTIOCTOP,&top) != 0)
               *ierr = 3;
            if (*ierr != 0) i = *count;
            }
         top.mt_op = MTNOP;
         }
                                        /* WEOF and BEGW             */
      else if ((strcmp(op,"WEOF") == 0) || (strcmp(op,"BEGW") == 0)) {
         if (strcmp(op,"BEGW") == 0) {
            top.mt_op = MTBSF;
            top.mt_count = 1;
            status = ioctl(*fcb,MTIOCTOP,&top);
            if (status != 0 && stat.mt_fileno != 0) {
               *ierr = 3;
               }
            else {
               if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
                  *ierr = 3;
                  printf("ZTAP2: error getting drive status\n");
                  perror("ZTAP2");
                  }
               else {
#if MT_Debug                              /* NOTE - debug            */
                  printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                     "BSF1",top.mt_op,*ierr);
                  printf("ZTAP2: stat.mt_type = %d\n",
                     stat.mt_type);
                  printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
                     stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
                  printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
                     stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
                  printf("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
                     stat.mt_fileno,stat.mt_blkno);
#endif
                  if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
                     *ierr = 5;
                  else if (stat.mt_type == MT_ISXY) {
                     if ((stat.mt_dsreg & XTS_BOT) != 0)
                        *ierr = 5;
                     else if (stat.mt_erreg == XTE_BOT)
                        *ierr = 5;
                     }
                  else {                      /* SCSI tape.  */
                     if (stat.mt_erreg == SC_BOT)
                        *ierr = 5;
                     }
                  }
               }
            }
         if (*ierr == 0) {
            top.mt_op = MTWEOF;
            top.mt_count = 1;
                                             /* 2 EOFs 1/2 inch */
            if (stat.mt_type == MT_ISXY) top.mt_count += 1;
            if (stat.mt_type == MT_ISHP) top.mt_count += 1;
            if (stat.mt_type == MT_ISKENNEDY) top.mt_count += 1;
            if (stat.mt_type == ST_TYPE_HIC) top.mt_count += 1;
            if (stat.mt_type == ST_TYPE_REEL) top.mt_count += 1;
            status = ioctl(*fcb,MTIOCTOP,&top);
            if (status != 0) {
               *ierr = 3;
               }
            else {
               if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
                  *ierr = 3;
                  printf("ZTAP2: error getting drive status\n");
                  perror("ZTAP2");
                  }
               else {
#if MT_Debug                              /* NOTE - debug            */
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
#endif
                  top.mt_count -= 1;
                  if (stat.mt_type == MT_ISXY) top.mt_count += 2;
                  top.mt_op = MTNOP;
                  if (top.mt_count) top.mt_op = MTBSF;
                  status = ioctl (*fcb, MTIOCTOP, &top);
                  if (status != 0 && stat.mt_fileno != 0) *ierr = 3;
                  if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
                     *ierr = 3;
                     printf("ZTAP2: error getting drive status\n");
                     perror("ZTAP2");
                     }
                  else {
#if MT_Debug                              /* NOTE - debug            */
                     printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                        "BSF ",top.mt_op,*ierr);
                     printf("ZTAP2: stat.mt_type = %d\n",
                        stat.mt_type);
                     printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
                        stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
                     printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
                        stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
                     printf(
                        "ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
                        stat.mt_fileno,stat.mt_blkno);
#endif
                     top.mt_op = MTNOP;
                     }
                  }
               }
            }
         }
                                        /* MTEOM while Exabyte is  */
                                        /* at EOM causes problems  */
      else if ((strcmp(op,"EOM ") == 0) || (strcmp(op,"AEOI") == 0)) {
         top.mt_count = 1;
                                             /* 1/2 inch's differ  */
         if (stat.mt_type == MT_ISXY) top.mt_count += 1;
         if (stat.mt_type == MT_ISHP) top.mt_count += 1;
         if (stat.mt_type == MT_ISKENNEDY) top.mt_count += 1;
         if (stat.mt_type == ST_TYPE_HIC) top.mt_count += 1;
         if (stat.mt_type == ST_TYPE_REEL) top.mt_count += 1;
                                             /* DATs and Exabytes  */
         if (top.mt_count == 1) {
            top.mt_op = MTBSF;
            status = ioctl(*fcb,MTIOCTOP,&top);
            if (status != 0 && stat.mt_fileno != 0) *ierr = 3;
            top.mt_op = MTEOM;
            }
                                            /* 1/2-inch by hand */
         else {
            top.mt_count = 1;
            top.mt_op = MTFSF;
            for (jc = 0; jc < 40000; jc++) {
               if ((nr = read (*fcb, record, 32768)) == 0) break;
               if (ioctl (*fcb, MTIOCTOP, &top) == -1) break;
               }
            if (jc >= 40000) {
               *ierr = 3;
               sprintf (msg, "ZTAP2 advance 40000 files with no EOI");
               zmsgwr_ (msg, &n7);
               }
            top.mt_op = MTNOP;
            }
         }
      else *ierr = 2;

      if (*ierr == 0) {
         status = ioctl(*fcb,MTIOCTOP,&top);
         if (status != 0) {
/*          Trap error return from MTBSF with tape at file 0.         */
            if (top.mt_op == MTBSF && stat.mt_fileno == 0) status = 0;
            }

         if (status != 0) {
            *ierr = 3;
            }
         else if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTAP2: error getting drive status\n");
            perror("ZTAP2");
            }
         else {
#if MT_Debug                              /* NOTE - debug            */
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
#endif
            }

         if (strcmp(op,"AEOI") == 0) {
            filenumb = 0x00FFFFFF & stat.mt_fileno;
            *count = filenumb + 1;
            }

         if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
            *ierr = 5;
         else if (stat.mt_type == MT_ISXY) {
            if ((stat.mt_dsreg & XTS_BOT) != 0)
               *ierr = 5;
            else if ((stat.mt_dsreg & XTS_EOT) != 0)
               *ierr = 6;
            if (stat.mt_erreg == XTE_BOT)
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

                                        /* advance over last EOF   */
         if ((top.mt_op == MTBSF) && (*ierr == 0)) {
            top.mt_op = MTFSF;
            top.mt_count = 1;
            status = ioctl(*fcb,MTIOCTOP,&top);
            if (status != 0) {
               *ierr = 3;
               }
            else if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
               *ierr = 3;
               printf("ZTAP2: error getting drive status\n");
               perror("ZTAP2");
               }
#if MT_Debug                              /* NOTE - debug            */
            else {
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
               printf("ZTAP2: stat_mt_flags = %d\n",stat.mt_flags);
               }
#endif
            }
         }

      if (*ierr == 3)
         *(fcb + 2) = errno;
      else
         *(fcb + 2) = 0;
      }

   return;
}
