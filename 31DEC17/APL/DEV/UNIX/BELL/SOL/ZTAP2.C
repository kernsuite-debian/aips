#define Z_tap2__
                                        /* Must not claim POSIX_SOURCE*/
                                        /* types.h would not define   */
                                        /* things needed in mtio.h    */
#ifdef _POSIX_SOURCE
#undef _POSIX_SOURCE
#endif

#define MT_Debug 0                      /* debug message control      */

#include <unistd.h>
#include <sys/types.h>
#include <string.h>
#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#include <errno.h>
#include <AIPSPROTO.H>

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

#if __STDC__
   void ztap2_(char opr[4], int *count, int *fcb, int *ierr)
#else
   void ztap2_(opr, count, fcb, ierr)
   int *count, *fcb, *ierr;
   char opr[4];
#endif
/*--------------------------------------------------------------------*/
/*! tape movements as called by ZTAPE                                 */
/*# Tape                                                              */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1999                                          */
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
/*    count  I     Number of times to do operation                    */
/*  In/out:                                                           */
/*    fcb    I(*)  File descriptor from FTAB file control block       */
/*  Output:                                                           */
/*    ierr   I     Error code: 0 = success                            */
/*                    2 = input specification error                   */
/*                    3 = i/o error                                   */
/*                    4 = tape mark encountered                       */
/*                    5 = tape at load point                          */
/*                    6 = tape at physical end                        */
/* SUN Exabyte version                                                */
/*  Modified to do nothing for real-time (fd < 0) "tapes"             */
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd;
   struct mtop top;
   struct mtget stat;
   char op[5];
   long   filenumb;
   int i, nr, status;
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
      else if (strcmp(op,"ADVF") == 0) {
         top.mt_op = MTFSF;
         }
                                        /* BSF for 1/2 inch, NBSF  */
                                        /* for cartr., DAT, Exabyte*/
      else if (strcmp(op,"BAKF") == 0) {
         top.mt_op = MTNBSF;
#ifdef MT_ISXY
         if (stat.mt_type == MT_ISXY) top.mt_op = MTBSF;
#endif
         if (stat.mt_type == MT_ISHP) top.mt_op = MTBSF;
         if (stat.mt_type == MT_ISKENNEDY) top.mt_op = MTBSF;
         if (stat.mt_type == ST_TYPE_HIC) top.mt_op = MTBSF;
         if (stat.mt_type == ST_TYPE_REEL) top.mt_op = MTBSF;
         if (top.mt_op == MTNBSF) top.mt_count--;
         }
                                       /* Only way to detect tape */
                                       /* marks.                  */
      else if (strcmp(op,"ADVR") == 0) {
         top.mt_op = MTFSR;
         }
                                        /* Only way to detect tape */
                                        /* marks.  UGLYYY          */
      else if (strcmp(op,"BAKR") == 0) {
         top.mt_op = MTBSR;
         }
                                        /* WEOF and BEGW             */
      else if ((strcmp(op,"WEOF") == 0) || (strcmp(op,"BEGW") == 0)) {
         if (strcmp(op,"BEGW") == 0) {
            top.mt_op = MTNBSF;
            top.mt_count = 0;
#ifdef MT_ISXY
            if (stat.mt_type == MT_ISXY) top.mt_op = MTBSF;
#endif
            if (stat.mt_type == MT_ISHP) top.mt_op = MTBSF;
            if (stat.mt_type == MT_ISKENNEDY) top.mt_op = MTBSF;
            if (stat.mt_type == ST_TYPE_HIC) top.mt_op = MTBSF;
            if (stat.mt_type == ST_TYPE_REEL) top.mt_op = MTBSF;
            if (top.mt_op == MTBSF) top.mt_count = 1;
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
                  printf
                     ("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
                     stat.mt_fileno,stat.mt_blkno);
#endif
                  if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
                     *ierr = 5;
#ifdef MT_ISXY
                  else if (stat.mt_type == MT_ISXY) {
                     if ((stat.mt_dsreg & XTS_BOT) != 0)
                        *ierr = 5;
                     else if (stat.mt_erreg == XTE_BOT)
                        *ierr = 5;
                     }
#endif
                  else {                      /* SCSI tape.  */
                     if (stat.mt_erreg == SC_BOT)
                        *ierr = 5;
                     }
                  }
               }
                                          /* back over EOF if needed */
            if ((top.mt_op == MTNBSF) && (*ierr == 0)) {
               top.mt_count = 1;
               top.mt_op = MTBSF;
               if ((status = ioctl(*fcb,MTIOCTOP,&top)) != 0) {
                  if (errno != ENOTTY) {
                     *ierr = 3;
                      }
                  else {
                                    /* One should be able to NBSF 1  */
                                    /* then FSR (large #) to get just*/
                                    /* before the EOF.  But it does  */
                                    /* not work.  Skip the BEGW EOF  */
                                    /* entirely with okay error code */
                     *ierr = 4;
                     }
                  }
               if (*ierr == 0) {
                  if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
                     *ierr = 3;
                     printf("ZTAP2: error getting drive status\n");
                     perror("ZTAP2");
                     }
                  else {
#if MT_Debug                              /* NOTE - debug            */
                     printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                        "BSF2",top.mt_op,*ierr);
                     printf("ZTAP2: stat.mt_type = %d\n",
                        stat.mt_type);
                     printf("ZTAP2: stat.mt_dsreg = %o %d %x\n",
                        stat.mt_dsreg,stat.mt_dsreg,stat.mt_dsreg);
                     printf("ZTAP2: stat.mt_erreg = %o %d %x\n",
                       stat.mt_erreg,stat.mt_erreg,stat.mt_erreg);
                     printf
                        ("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
                        stat.mt_fileno,stat.mt_blkno);
#endif
                     if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
                        *ierr = 5;
#ifdef MT_ISXY
                     else if (stat.mt_type == MT_ISXY) {
                        if ((stat.mt_dsreg & XTS_BOT) != 0)
                           *ierr = 5;
                        else if (stat.mt_erreg == XTE_BOT)
                           *ierr = 5;
                        }
#endif
                     else {                      /* SCSI tape.  */
                        if (stat.mt_erreg == SC_BOT)
                           *ierr = 5;
                        }
                     }
                  }
               }
            }
         if (*ierr == 0) {
            top.mt_op = MTWEOF;
            top.mt_count = 1;
                                         /* 2 EOFs 1/2 inch          */
#ifdef MT_ISXY
            if (stat.mt_type == MT_ISXY) top.mt_count += 1;
#endif
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
                  printf
                     ("ZTAP2: stat.mt_filno = %d stat.mt_blkno = %d\n",
                     stat.mt_fileno,stat.mt_blkno);
#endif
                  top.mt_op = MTBSF;                      /* ??????*/
#ifdef MT_ISXY
                  if (stat.mt_type == MT_ISXY) top.mt_count += 2;
#endif
                  if (top.mt_count == 1) top.mt_op = MTNBSF;
                  if (top.mt_op == MTNBSF) top.mt_count = 0;
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
                     top.mt_count = 1;
                     if (top.mt_op == MTNBSF)
                        top.mt_op = MTNOP;
                     else
                        top.mt_op = MTFSF;
                     if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
                        *ierr = 5;
#ifdef MT_ISXY
                     else if (stat.mt_type == MT_ISXY) {
                        if ((stat.mt_dsreg & XTS_BOT) != 0)
                           *ierr = 5;
                        else if (stat.mt_erreg == XTE_BOT)
                           *ierr = 5;
                        }
#endif
                     else {                      /* SCSI tape.  */
                        if (stat.mt_erreg == SC_BOT)
                           *ierr = 5;
                        }
                     }
                  }
               }
            }
         }
                                        /* MTEOM while Exabyte is  */
                                        /* at EOM causes problems  */
      else if ((strcmp(op,"EOM ") == 0) || (strcmp(op,"AEOI") == 0)) {
         top.mt_op = MTBSF;                      /* ??????*/
         top.mt_op = MTNBSF;
#ifdef MT_ISXY
         if (stat.mt_type == MT_ISXY) top.mt_op = MTBSF;
#endif
         if (stat.mt_type == MT_ISHP) top.mt_op = MTBSF;
         if (stat.mt_type == MT_ISKENNEDY) top.mt_op = MTBSF;
         if (stat.mt_type == ST_TYPE_HIC) top.mt_op = MTBSF;
         if (stat.mt_type == ST_TYPE_REEL) top.mt_op = MTBSF;
         if (top.mt_op == MTNBSF) top.mt_count = 0;
         status = ioctl(*fcb,MTIOCTOP,&top);
         if (status != 0 && stat.mt_fileno != 0) *ierr = 3;
         top.mt_op = MTEOM;
         }
      else *ierr = 2;

      if (*ierr == 0) {
         status = ioctl(*fcb,MTIOCTOP,&top);
         if (status != 0) {
/*          Trap error return from MTBSF with tape at file 0.         */
            if (top.mt_op == MTBSF && stat.mt_fileno == 0) status = 0;
            if (top.mt_op == MTNBSF && stat.mt_fileno == 0) status = 0;
            }

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

         if ((stat.mt_fileno == 0) && (stat.mt_blkno == 0))
            *ierr = 5;
#ifdef MT_ISXY
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
#endif
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

         if (strcmp(op,"AEOI") == 0) {
            filenumb = 0x00FFFFFF & stat.mt_fileno;
            *count = filenumb + 1;
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
