#define Z_tap2__
                                        /* Must not claim POSIX_SOURCE*/
                                        /* types.h would not define   */
                                        /* things needed in mtio.h    */
#ifdef _POSIX_SOURCE
#undef _POSIX_SOURCE
#endif

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
   void ztap2_(char opr[4], int *count, int *fcb, int *ierr)
#else
   ztap2_(opr, count, fcb, ierr)
   int *count, *fcb, *ierr;
   char opr[4];
#endif
/*--------------------------------------------------------------------*/
/*! tape movements as called by ZTAPE                                 */
/*# Z2 Tape                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1998                                         */
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
/*    count  I     Number of times to do operation                    */
/*    fcb    I(*)  File descriptor from FTAB file control block       */
/*  Output:                                                           */
/*    ierr   I     Error code: 0 = success                            */
/*                    2 = input specification error                   */
/*                    3 = i/o error                                   */
/*                    4 = tape mark encountered                       */
/*                    5 = tape at load point                          */
/*                    6 = tape at physical end                        */
/*   DEC UNIX version - 4.2 bsd (grossly system/device dependent).    */
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd;
   struct mtop top;
   struct mtget stat;
   struct devget devs;
   char op[5], record[32768];
   int i, nr, status, jerr, atbom, ztp2st();
/*--------------------------------------------------------------------*/
   errno = 0;
   for (i = 0; i < 4; i++)
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
   else if (ioctl(*fcb,DEVIOCGET,&devs) != 0) {
      *ierr = 3;
      printf("ZTAP2: error getting drive device status\n");
      perror("ZTAP2");
      }
   else {
#if MT_Debug                              /* NOTE - debug            */
      printf("ZTAP2 at NOP-0: op = %s top.mt_op = %d ierr = %d\n",
         op,top.mt_op,*ierr);
      printf("ZTAP2: stat.mt_dsreg, erreg (hex) = %x , %x\n",
         stat.mt_dsreg,stat.mt_erreg);
      printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
         stat.mt_type,stat.mt_resid);
      printf("ZTAP2: devs.interf = %s, devs.dev = %s\n",
         devs.interface, devs.device);
      printf("ZTAP2: devs.stat = %x, devs.cat_stat = %x\n",
         devs.stat, devs.category_stat);
#endif
      atbom = (devs.stat & DEV_BOM);
      if (strcmp(op,"REWI") == 0) {
         top.mt_op = MTREW;
         if (atbom) *ierr = 5;
         }
      else if (strcmp(op,"ADVF") == 0)
         top.mt_op = MTFSF;
      else if (strcmp(op,"BAKF") == 0) {
         top.mt_op = MTBSF;
         if (atbom) *ierr = 5;
         }
      else if (strcmp(op,"ADVR") == 0)
         top.mt_op = MTFSR;
      else if (strcmp(op,"BAKR") == 0) {
         top.mt_op = MTBSR;
         if (atbom) *ierr = 5;
         }
                                        /* WEOF and BEGW             */
      else if ((strcmp(op,"WEOF") == 0) || (strcmp(op,"BEGW") == 0)) {
         if (strcmp(op,"BEGW") == 0) {
            if (atbom) *ierr = 5;
            else {
               top.mt_op = MTBSF;
               top.mt_count = 1;
               status = ioctl(*fcb,MTIOCTOP,&top);
               if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
                  *ierr = 3;
                  printf("ZTAP2: error getting drive status\n");
                  perror("ZTAP2");
                  }
               else if (ioctl(*fcb,DEVIOCGET,&devs) != 0) {
                  *ierr = 3;
                  printf("ZTAP2: error getting drive device status\n");
                  perror("ZTAP2");
                  }
               else {
#if MT_Debug                              /* NOTE - debug            */
                  printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                     "BSF1",top.mt_op,*ierr);
                  printf("ZTAP2: stat.mt_dsreg, erreg (hex) = %x , %x\n",
                     stat.mt_dsreg,stat.mt_erreg);
                  printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                     stat.mt_type,stat.mt_resid);
                  printf("ZTAP2: I/O op status = %d\n", status);
                  printf("ZTAP2: devs.interf = %s, devs.dev = %s\n",
                     devs.interface, devs.device);
                  printf("ZTAP2: devs.stat = %x, devs.cat_stat = %x\n",
                     devs.stat, devs.category_stat);
#endif
                  jerr = ztp2st (&stat, &devs);
                  if ((status == -1) && (jerr == 0)) jerr = 5;
                  if (jerr == 5)
                     *ierr = 5;
                  else
                     if (status != 0) *ierr = 3;
                  }
               }
            }
         if (*ierr == 0) {
            top.mt_op = MTWEOF;
            top.mt_count = 1;
                                         /* 2 EOFs 1/2 inch, more ?? */
            if (stat.mt_type == MT_ISTS) top.mt_count += 1;
            if (stat.mt_type == MT_ISHT) top.mt_count += 1;
            if (stat.mt_type == MT_ISTM) top.mt_count += 1;
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
               else if (ioctl(*fcb,DEVIOCGET,&devs) != 0) {
                  *ierr = 3;
                  printf("ZTAP2: error getting drive device status\n");
                  perror("ZTAP2");
                  }
               else {
#if MT_Debug                              /* NOTE - debug            */
                  printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                     op,top.mt_op,*ierr);
                  printf("ZTAP2: stat.mt_dsreg, erreg (hex) = %x , %x\n",
                     stat.mt_dsreg,stat.mt_erreg);
                  printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                     stat.mt_type,stat.mt_resid);
                  printf("ZTAP2: devs.interf = %s, devs.dev = %s\n",
                     devs.interface, devs.device);
                  printf("ZTAP2: devs.stat = %x, devs.cat_stat = %x\n",
                     devs.stat, devs.category_stat);
#endif
                  top.mt_op = MTBSF;                      /* ??????*/
                  top.mt_count -= 1;
                  if (top.mt_count) {
                     status = ioctl (*fcb, MTIOCTOP, &top);
                     if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
                        *ierr = 3;
                        printf("ZTAP2: error getting drive status\n");
			perror("ZTAP2");
                        }
                     else if (ioctl(*fcb,DEVIOCGET,&devs) != 0) {
                        *ierr = 3;
                        printf("ZTAP2: error getting drive device status\n");
                        perror("ZTAP2");
                        }
                     else {
#if MT_Debug                              /* NOTE - debug            */
                        printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                           "BSF ",top.mt_op,*ierr);
                        printf("ZTAP2: stat.mt_dsreg, erreg (hex) = %x , %x\n",
                           stat.mt_dsreg,stat.mt_erreg);
                        printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                           stat.mt_type,stat.mt_resid);
                        printf("ZTAP2: I/O op status = %d\n", status);
                        printf("ZTAP2: devs.interf = %s, devs.dev = %s\n",
                           devs.interface, devs.device);
                        printf("ZTAP2: devs.stat = %x, devs.cat_stat = %x\n",
                           devs.stat, devs.category_stat);
#endif
                        }
                     }
                  top.mt_count = 1;
                  top.mt_op = MTNOP;
	          }
               }
            }
         }
      else if ((strcmp(op,"EOM ") == 0) || (strcmp(op,"AEOI") == 0)) {
         if (stat.mt_type == MT_ISSCSI) {
            top.mt_op = MTSEOD;
            top.mt_count = 1;
                                        /* can't know count       */
            *count = 0;
            }
                                        /* FNDEOT in C             */
         else {
            for (i = 0; i < 32000; i++) {
               nr = read (*fcb, record, 32768);
               if (nr <= 0) {
                  if (nr < 0) {
                     *ierr = 3;
                     if (errno == ENOSPC) *ierr = 6 ;
                     }
                  break;
                  }
               else {
                  top.mt_op = MTFSF;
                  top.mt_count = 1;
                  status = ioctl(*fcb,MTIOCTOP,&top);
                  if (status != 0) {
                     *ierr = 3;
                     break;
                     }
                  }
               }
            *count = -i - 1;
            if (*ierr == 3) {
               if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
                  *ierr = 3;
                  printf("ZTAP2: error getting drive status\n");
                  perror("ZTAP2");
                  }
               else if (ioctl(*fcb,DEVIOCGET,&devs) != 0) {
                  *ierr = 3;
                  printf("ZTAP2: error getting drive device status\n");
                  perror("ZTAP2");
                  }
#if MT_Debug                              /* NOTE - debug            */
               else {
                  printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                     op,top.mt_op,*ierr);
                  printf("ZTAP2: stat.mt_dsreg, erreg (hex) = %x , %x\n",
                     stat.mt_dsreg,stat.mt_erreg);
                  printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                     stat.mt_type,stat.mt_resid);
                  printf("ZTAP2: I/O op status = %d\n", status);
                  printf("ZTAP2: devs.interf = %s, devs.dev = %s\n",
                     devs.interface, devs.device);
                  printf("ZTAP2: devs.stat = %x, devs.cat_stat = %x\n",
                     devs.stat, devs.category_stat);
                  }
#endif
               jerr = ztp2st (&stat, &devs);
               if (jerr != 0) *ierr = jerr;
               }
            top.mt_op = MTBSF;
            top.mt_count = 2;
            if (*ierr == 6) top.mt_count = 1;
            if ((*ierr == 6) || (*ierr == 4)) *ierr = 0;
            }
         }
      else *ierr = 2;

      if (*ierr == 0) {
         status = ioctl (*fcb, MTIOCTOP, &top);
         if ((status != 0) && ((top.mt_op != MTBSF) || (status != -1)))
            *ierr = 3;
         else if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTAP2: error getting drive status\n");
            perror("ZTAP2");
            }
         else if (ioctl(*fcb,DEVIOCGET,&devs) != 0) {
            *ierr = 3;
            printf("ZTAP2: error getting drive device status\n");
            perror("ZTAP2");
            }
#if MT_Debug                              /* NOTE - debug            */
         else {
            printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
               op,top.mt_op,*ierr);
            printf("ZTAP2: stat.mt_dsreg, erreg (hex) = %x , %x\n",
               stat.mt_dsreg,stat.mt_erreg);
            printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
               stat.mt_type,stat.mt_resid);
            printf("ZTAP2: I/O op status = %d\n", status);
            printf("ZTAP2: devs.interf = %s, devs.dev = %s\n",
               devs.interface, devs.device);
            printf("ZTAP2: devs.stat = %x, devs.cat_stat = %x\n",
               devs.stat, devs.category_stat);
            }
#endif
                                            /* kludge for BOT error */
         if ((status == -1) && (top.mt_op == MTBSF) && (*ierr == 0) &&
            (errno == 5) && ((stat.mt_dsreg == 3) || (stat.mt_dsreg == 11))
            && (stat.mt_erreg == 64)) {
            *ierr = 5;
            top.mt_count = 1;
            top.mt_op = MTCSE;          /* and clear error status */
            if (ioctl(*fcb,MTIOCTOP,&top) != 0)
               perror("ZTAP2 CSE -");
            top.mt_op = MTCLX;
            if (ioctl(*fcb,MTIOCTOP,&top) != 0)
               perror("ZTAP2 CSE -");
            }
         else {
            jerr = ztp2st (&stat, &devs);
            if (jerr != 0) *ierr = jerr;
            }
         if ((status == -1) && (top.mt_op == MTBSF) && (*ierr == 0))
            *ierr = 5;
                                        /* advance over last EOF   */
         if ((top.mt_op == MTBSF) && ((*ierr == 0) || (*ierr == 4))) {
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
            else if (ioctl(*fcb,DEVIOCGET,&devs) != 0) {
               *ierr = 3;
               printf("ZTAP2: error getting drive device status\n");
               perror("ZTAP2");
               }
#if MT_Debug                              /* NOTE - debug            */
            else {
               printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                  op,top.mt_op,*ierr);
               printf("ZTAP2: stat.mt_type = %d\n", stat.mt_type);
               printf("ZTAP2: stat.mt_dsreg, erreg (hex) = %x , %x\n",
                  stat.mt_dsreg,stat.mt_erreg);
               printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                  stat.mt_type,stat.mt_resid);
               printf("ZTAP2: devs.interf = %s, devs.dev = %s\n",
                  devs.interface, devs.device);
               printf("ZTAP2: devs.stat = %x, devs.cat_stat = %x\n",
                  devs.stat, devs.category_stat);
               }
#endif
            }
         }

      if (*ierr == 3) {
         *(fcb + 2) = errno;
         fprintf(stderr,"ZTAP2 stat.mt_dsreg %d\n", stat.mt_dsreg);
         fprintf(stderr,"ZTAP2 stat.mt_erreg %d\n", stat.mt_erreg);
         }
      else
         *(fcb + 2) = 0;
      }

   return;
}

#if __STDC__
   int ztp2st (struct mtget *stat, struct devget *devs)
#else
   int ztp2st (stat, devs)
   struct mtget *stat;
   struct devget *devs;
#endif
{
   int jerr;
/*                                             HT drives     */
   jerr = 0;
   if (errno == ENOSPC)
      jerr = 6;
   else {
      if (stat->mt_type == MT_ISHT) {
         if ((stat->mt_dsreg & ERR) != 0) {
            jerr = 3;
            if ((stat->mt_erreg & (NERR1 | NERR2)) != 0) {
               if ((stat->mt_dsreg & BOT) != 0) jerr = 5;
               else if ((stat->mt_dsreg & EOT) != 0) jerr = 6;
               else if ((stat->mt_dsreg & TM ) != 0) jerr = 4;
               }
            else if ((stat->mt_erreg & (NERR3 | NERR4)) != 0) {
               if ((stat->mt_dsreg & BOT) != 0) jerr = 5;
               }
            }
         }
/*                                           TM drives      */
      else {
         if ((stat->mt_dsreg & ERR3) != 0) jerr = 3;
         if ((stat->mt_erreg & BOT3) != 0) jerr = 5;
         else if ((stat->mt_erreg & EOT3) != 0) jerr = 6;
         else if ((stat->mt_erreg & TM3 ) != 0) jerr = 4;
         }
      }

   if ((devs->stat & DEV_BOM) != 0) jerr = 5;
   if ((devs->stat & DEV_EOM) != 0) jerr = 6;
   if ((devs->category_stat & DEV_TPMARK) != 0) jerr = 4;

#if MT_Debug                              /* NOTE - debug            */
   if (jerr != 0) {
      fprintf(stderr,"ZTAP2 stat.mt_dsreg %x\n", stat->mt_dsreg);
      fprintf(stderr,"ZTAP2 stat.mt_erreg %x\n", stat->mt_erreg);
      fprintf(stderr,"ZTAP2 devs.stat %x\n", devs->stat);
      fprintf(stderr,"ZTAP2 devs.category_stat %x\n", devs->category_stat);
      fprintf(stderr,"ZTAP2 errno %d\n", errno);
      fprintf(stderr,"ZTAP2 jerr %d\n", jerr);
      }
#endif

   return (jerr);
}
