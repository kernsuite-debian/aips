#include <errno.h>
#include <stdio.h>
#include <strings.h>
#include <sys/types.h>
#include <sys/ioctl.h>
#include <sys/mtio.h>
#ifdef _POSIX_SOURCE
#include <unistd.h>
#endif

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
/*  In/Output:                                                        */
/*     msg      C*80   IN: task name                                  */
/*                     OUT: Error message buffer                      */
/*  Output:                                                           */
/*     syserr   I      System error code: 0 => no error               */
/*     ierr     I      Error return code: 0 => no error               */
/*                        1 => error                                  */
/*  Convex version - uses ZCRLOG                                      */
/*--------------------------------------------------------------------*/
int *mount, *idrive, *idens, *ntaped, *npops, *nuser, *syserr, *ierr;
char msg[];
/*--------------------------------------------------------------------*/
{
   extern int errno;
   int density, n, ll, lt, i4err;
   register int i;
   int use_tpmount, tfd;
   char cmd[256], mt0x[5], tlog[12], task[6];
   struct mtop top;
   struct mtget stat;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *syserr = 0;
   errno = 0;
   for (i=0; i < 5 && msg[i] != ' '; i++)
      task[i] = msg[i];
   task[i] = '\0';

                                        /* Convex OS 8.0 and later    */
                                        /* has a different tape       */
                                        /* interface to older systems */
                                        /* --- find out which to use  */
                                        /* (the newer interface is    */
                                        /* used if it is available):  */
   if ( access("/usr/convex/tpmount", X_OK) == 0 )
      use_tpmount = 1;
   else
      use_tpmount = 0;
 
                                        /* MOUNT or DISMOUNT?         */
   if (*mount == 1) {
                                        /* Determine density factor   */
      density = 0;
      if (*idens == 800) density = 1;
      else if (*idens == 1600) density = 2;
      else if (*idens == 6250) density = 3;
      else if (*idens == 22500) density = 3;
                                        /* Density legal?             */
      if (density == 0) {
         sprintf (msg, "ZMOUN2: INVALID DENSITY = %d",*idens);
         *ierr = 1;
         goto exit;
         }
                                        /* Calculate N for /dev/rmtN  */
      n = *idrive + (8 * density) - 5;
      if (n < 6) {
         sprintf (msg, "ZMOUN2: DENSITY %d INVALID FOR TAPE DRIVE %d",
            *idens, *idrive);
         *ierr = 1;
         goto exit;
         }
                                        /* Form system mount command: */
      if (use_tpmount)
	 sprintf (cmd,
	    "tpmount -a /dev/rmt%d -b -s $AIPS_ROOT/%s%X.USERNO%d.INTAPE%d",
	    n, task, *npops, *nuser, *idrive);
      else
         sprintf (cmd,
            "tpalloc -f /dev/rmt%d $AIPS_ROOT/%s%X.USERNO%d.INTAPE%d",
            n, task, *npops, *nuser, *idrive);
                                        /* Issue mount request        */
      if (system(cmd) != 0) {
         sprintf (msg, "ZMOUN2: ALLOCATION ERROR FOR TAPE DRIVE %d",
            *idrive);
         *syserr = errno;
         system("tpq");
         *ierr = 1;
         goto exit;
         }
      else system("tpq");
                                        /* Assign shell variable AMT0n*/
                                        /* "/dev/rmtMM"               */
      sprintf(mt0x,"AMT0%x",*idrive);
      ll = 5;
      sprintf(tlog,"/dev/rmt%d",n);
      lt = 12;
      zcrlog_(&ll, mt0x, &lt, tlog, &i4err);
      if (i4err != 0) {
         sprintf (msg, "ZMOUN2: CREATE LOGICAL ERROR FOR %s = %s",
            mt0x, tlog);
         *ierr = 1;
         goto exit;
         }
      if (*ierr == 0) {
         tfd = open (tlog, 0) ;
         if (tfd == -1) {
                                        /* Device doesn't exist.      */
            if (errno == ENOENT)
               *ierr = 2;
                                        /* Some other open error.     */
            else
               *ierr = 6;
            *syserr = errno;
            sprintf (msg, "ZMOUN2: Couldn't open tape device %s", 
               tlog);
            goto cleanup;
            }
         top.mt_count = 1;
         top.mt_op = MTNOP;
         ioctl (tfd, MTIOCTOP, &top);
         if (ioctl (tfd, MTIOCGET, &stat) != 0) {
            stat.mt_type = -1;
            }
         close (tfd) ;
         switch (stat.mt_type) {
            case MT_ISTA :
               sprintf (msg, "Mounted on 1/2-inch reel tape drive");
               break;
            case MT_ISTC :
               sprintf (msg,
                  "GOOD LUCK: Mounted on cartridge tape drive");
               break;
            case MT_ISDAT :
               sprintf (msg, "GOOD LUCK: Mounted on DAT tape drive");
               break;
            case MT_ISTR :
               sprintf (msg, "GOOD LUCK: Mounted on type TR Tape");
               break;
            default :
               sprintf (msg,
                  "GOOD LUCK: Mounted unknown tape device %d",
                  stat.mt_type);
               break;
            }
         }
      }
   else {

cleanup:
                                        /* Form system dismount       */
                                        /* command:                   */
      if ( use_tpmount )
         sprintf (cmd, "tpunmount -s $AIPS_ROOT/%s%X.USERNO%d.INTAPE%d",
	    task, *npops, *nuser, *idrive);
      else
         sprintf (cmd,"tpdealloc $AIPS_ROOT/%s%X.USERNO%d.INTAPE%d",
            task, *npops, *nuser, *idrive);
                                        /* Issue dismount request     */
      if (system(cmd) != 0) {
         sprintf (msg,"ZMOUN2: DEALLOCATION ERROR FOR TAPE DRIVE %d",
            *idrive);
         *syserr = errno;
         system("tpq");
         *ierr = 1;
         goto exit;
         }
      else {
         system("tpq");

                                        /* DeAssign shell variable    */
                                        /* AMT0n done by ZMOUNT now   */
         }
      }

exit:
   return;
}
