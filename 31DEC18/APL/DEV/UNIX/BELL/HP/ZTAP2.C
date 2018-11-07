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

#if __STDC__
   void ztap2_(char opr[4], int *count, int *fcb, int *ierr)
#else
   ztap2_(opr, count, fcb, ierr)
   int *count, *fcb, *ierr;
   char opr[4];
#endif
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
/*    count  I     Number of times to do operation                    */
/*    fcb    I(*)  File descriptor from FTAB file control block       */
/*  Output:                                                           */
/*    ierr   I     Error code: 0 = success                            */
/*                    2 = input specification error                   */
/*                    3 = i/o error                                   */
/*                    4 = tape mark encountered                       */
/*                    5 = tape at load point                          */
/*                    6 = tape at physical end                        */
/* HP version                                                         */
/*--------------------------------------------------------------------*/
{
   extern int Z_fcbfd;
   struct mtop top;
   struct mtget stat;
   char op[5], record[32768];
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
      printf("ZTAP2: stat.mt_gstat, erreg (hex) = %x , %x\n",
         stat.mt_gstat,stat.mt_erreg);
      printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
         stat.mt_type,stat.mt_resid);
      printf("ZTAP2: stat.mt_dsreg = %x %x\n",
         stat.mt_dsreg1,stat.mt_dsreg2);
#endif
      if (strcmp(op,"REWI") == 0)
         top.mt_op = MTREW;
      else if (strcmp(op,"ADVF") == 0) {
         top.mt_op = MTFSF;
         }
                                        /* BSF                     */
      else if (strcmp(op,"BAKF") == 0) {
         top.mt_op = MTBSF;
         }
                                        /* EOF detection uncertain */
      else if (strcmp(op,"ADVR") == 0) {
         top.mt_op = MTFSR;
         }
                                        /* EOF detection uncertain */
      else if (strcmp(op,"BAKR") == 0) {
         top.mt_op = MTBSR;
         }
                                        /* WEOF and BEGW             */
      else if ((strcmp(op,"WEOF") == 0) || (strcmp(op,"BEGW") == 0)) {
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
#if MT_Debug                              /* NOTE - debug            */
               printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                  "BSF1",top.mt_op,*ierr);
               printf("ZTAP2: stat.mt_gstat, erreg (hex) = %x , %x\n",
                  stat.mt_gstat,stat.mt_erreg);
               printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                  stat.mt_type,stat.mt_resid);
               printf("ZTAP2: stat.mt_dsreg = %x %x\n",
                  stat.mt_dsreg1,stat.mt_dsreg2);
#endif
               if (GMT_BOT(stat.mt_gstat)) *ierr = 5;
               if ((status != 0) && (*ierr == 0)) *ierr = 3;
               }
            }
         if (*ierr == 0) {
            top.mt_op = MTWEOF;
            top.mt_count = 1;
                                         /* 2 EOFs 1/2 inch          */
            if (stat.mt_type == MT_ISTS) top.mt_count += 1;
            if (stat.mt_type == MT_ISHT) top.mt_count += 1;
            if (stat.mt_type == MT_ISTM) top.mt_count += 1;
            if (stat.mt_type == MT_IS7970E) top.mt_count += 1;
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
                  printf("ZTAP2: stat.mt_gstat, erreg (hex) = %x , %x\n",
                     stat.mt_gstat,stat.mt_erreg);
                  printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                     stat.mt_type,stat.mt_resid);
                  printf("ZTAP2: stat.mt_dsreg = %x %x\n",
                     stat.mt_dsreg1,stat.mt_dsreg2);
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
                     else {
#if MT_Debug                              /* NOTE - debug            */
                        printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                           "BSF ",top.mt_op,*ierr);
                        printf("ZTAP2: stat.mt_gstat, erreg (hex) = %x , %x\n",
                           stat.mt_gstat,stat.mt_erreg);
                        printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                           stat.mt_type,stat.mt_resid);
                        printf("ZTAP2: stat.mt_dsreg = %x %x\n",
                           stat.mt_dsreg1,stat.mt_dsreg2);
#endif
                        }
                     }
                  top.mt_count = 1;
                  top.mt_op = MTNOP;
	          }
               }
            }
         }
                                        /* FNDEOT in C             */
      else if ((strcmp(op,"EOM ") == 0) || (strcmp(op,"AEOI") == 0)) {
         if ((stat.mt_type == MT_ISDDS1) ||
            (stat.mt_type == MT_ISDDS2)) {
            top.mt_count = 1;
            top.mt_op = MTEOD;
                                          /* cannot get a file number */
            *count = 0;
	    }
         else {
            for (i = 0; i < 32000; i++) {
               nr = read (*fcb, record, 32768);
               if (nr <= 0) {
                  if (nr < 0) *ierr = 3;
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
#if MT_Debug                              /* NOTE - debug            */
               else {
                  printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                     op,top.mt_op,*ierr);
                  printf("ZTAP2: stat.mt_gstat, erreg (hex) = %x , %x\n",
                     stat.mt_gstat,stat.mt_erreg);
                  printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                     stat.mt_type,stat.mt_resid);
                  printf("ZTAP2: stat.mt_dsreg = %x %x\n",
                     stat.mt_dsreg1,stat.mt_dsreg2);
                  }
#endif
               if (GMT_BOT(stat.mt_gstat))
                  *ierr = 5;
               else if (GMT_EOD(stat.mt_gstat))
                  *ierr = 6;
               else if (GMT_EOT(stat.mt_gstat))
                  *ierr = 6;
               else if (GMT_EOF(stat.mt_gstat))
                  *ierr = 4;
               }
            top.mt_op = MTBSF;
            top.mt_count = 2;
            if (*ierr == 6) top.mt_count = 1;
            if ((*ierr == 6) || (*ierr == 4)) *ierr = 0;
            }
         }
      else *ierr = 2;

      if (*ierr == 0) {
         status = ioctl (*fcb,MTIOCTOP,&top);
         if (ioctl(*fcb,MTIOCGET,&stat) != 0) {
            *ierr = 3;
            printf("ZTAP2: error getting drive status\n");
            perror("ZTAP2");
            }
#if MT_Debug                              /* NOTE - debug            */
         else {
            printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
               op,top.mt_op,*ierr);
            printf("ZTAP2: stat.mt_gstat, erreg (hex) = %x , %x\n",
               stat.mt_gstat,stat.mt_erreg);
            printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
               stat.mt_type,stat.mt_resid);
            printf("ZTAP2: stat.mt_dsreg = %x %x\n",
               stat.mt_dsreg1,stat.mt_dsreg2);
            }
#endif
         if (GMT_BOT(stat.mt_gstat))
            *ierr = 5;
         else if (GMT_EOD(stat.mt_gstat))
            *ierr = 6;
         else if (GMT_EOT(stat.mt_gstat))
            *ierr = 6;
         else if (GMT_EOF(stat.mt_gstat))
            *ierr = 4;

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
#if MT_Debug                              /* NOTE - debug            */
            else {
               printf("ZTAP2: op = %s top.mt_op = %d ierr = %d\n",
                  op,top.mt_op,*ierr);
               printf("ZTAP2: stat.mt_type = %d\n", stat.mt_type);
               printf("ZTAP2: stat.mt_gstat, erreg (hex) = %x , %x\n",
               stat.mt_gstat,stat.mt_erreg);
               printf("ZTAP2: stat.mt_type = %d, out resid = %d\n",
                  stat.mt_type,stat.mt_resid);
               printf("ZTAP2: stat.mt_dsreg = %x %x\n",
                  stat.mt_dsreg1,stat.mt_dsreg2);
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
