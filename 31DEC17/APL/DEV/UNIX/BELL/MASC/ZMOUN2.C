#include <errno.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>

#define FILPRT 01000

zmoun2_(mount, idrive, idens, ntaped, npops, nuser, msg, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! mount or dismount magnetic tape device - lowest level Z           */
/*# Tape Z2                                                           */
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
/*  Issue a software tape mount or dismount requests.                 */
/*  Inputs:                                                           */
/*     mount    I      Operation indicator code:                      */
/*                        1 => mount                                  */
/*                        0 => dismount                               */
/*     idrive   I      Drive number                                   */
/*     idens    I      Density                                        */
/*     ntaped   I      Number of tape drives                          */
/*     npops    I      Pops number                                    */
/*     nuser    I      User AIPS number                               */
/*  Output:                                                           */
/*     msg      C*80   Error message buffer                           */
/*     syserr   I      System error code: 0 => no error               */
/*     ierr     I      Error return code: 0 => no error               */
/*                        1 => error                                  */
/*  Masscomp version.                                                 */
/*--------------------------------------------------------------------*/
int *mount, *idrive, *idens, *ntaped, *npops, *nuser, *syserr, *ierr;
char msg[];
/*--------------------------------------------------------------------*/
{
   extern int errno;
   FILE *pp, *popen();
   char cmd[80], shvar[10], device[65];
   int fd;
   struct mtop top;
   struct mtget stat;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *syserr = 0;
   errno = 0;
   if (*idrive < 1 || *idrive > *ntaped) {
      sprintf (msg,"ZMOUN2: DRIVE #%d DOES NOT EXIST!",*idrive);
      *ierr = 1;
      return;
      }
   else if (*mount == 1) {
      if (*idens != 1600) {
         sprintf (msg,"ZMOUN2: ONLY 1600 BPI ALLOWED!");
         *ierr = 1;
         return;
         }
      sprintf (shvar, "MT0%d", *idrive-1);
      sprintf (device, "%s", getenv(shvar));
      sprintf (cmd, "assign -m %d %d", *idrive-1, *idens);
      pp = popen (cmd, "r");
      fgets (msg, 80, pp);
      pclose (pp);
      if (msg[0] == 'U') *ierr = 1;
      else if ((fd = open (device, 0)) == -1) {
         *ierr = 1;
         *syserr = errno;
         }
      else {
         if (ioctl(fd,MTIOCGET,&stat) != 0) {
            *ierr = 1;
            *syserr = errno;
            }
         else if ((stat.mt_erreg & FILPRT) != 0)
            printf ("ZMOUN2: TAPE DRIVE #%d MOUNTED WRITE LOCKED!\n",
               *idrive);
         else
            printf ("ZMOUN2: TAPE DRIVE #%d MOUNTED WRITE ENABLED!\n",
               *idrive);
         top.mt_op = MTREW;
         ioctl (fd, MTIOCTOP, &top);
         close (fd);
         }
      }
   else {
      if ((fd = open(device,0)) == -1) {
         *ierr = 1;
         *syserr = errno;
         }
      else {
         top.mt_op = MTOFFL;
         ioctl (fd, MTIOCTOP, &top);
         close (fd);
         sprintf (cmd, "deassign -m %d", *idrive-1);
         system (cmd);
         }
      }

   return;
}
