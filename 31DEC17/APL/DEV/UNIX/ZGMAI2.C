#define Z_gmai2__
#define MAXCOM 256
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>
#include <AIPSPROTO.H>
#ifdef __STDC__
#if __STDC__ == 1
extern FILE *popen(const char *, const char *);
extern int  pclose(FILE *);
#endif
#endif

#if __STDC__
   void zgmai2_(char *file, int *lfil, int *stype, int *ierr)
#else
   void zgmai2_(file, lfil, stype, ierr)
   int *lfil, *stype, *ierr;
   char file[];
#endif
/*--------------------------------------------------------------------*/
/*! mail a file to aipsmail and delete it                             */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997, 2003, 2007                              */
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
/*  Mails "file" to aipsmail@nrao.edu and then deletes it             */
/*  Inputs:                                                           */
/*     file     C*(*)   the fully expanded file name                  */
/*     lfil     I       length of file                                */
/*     stype    I       How to get subject line: 0 -> -s " "          */
/*                         1 -> insert in file                        */
/*  Output:                                                           */
/*     ierr     I       error return: 0 means okay                    */
/*                         1 input command too long                   */
/*                         else = errno, the system error occurred    */
/*  UNIX version uses with popen "rmail"                              */
/*--------------------------------------------------------------------*/
{
   register int i, j;
   char cmd[512], cfil[MAXCOM], msgbuf[80], outbuf[132];
   int n7 = 7, n3 = 3;
   FILE *zptr;
   char *addr1 = "daip@nrao.edu";
/*--------------------------------------------------------------------*/
   *ierr = 0;
   if (*lfil >= MAXCOM)
      *ierr = 1;
                                        /*  make null-terminated      */
   else {
      j = 0;
      for (i = 0; i < MAXCOM && i < *lfil; i++) {
         cfil[i] = file[i];
         if (cfil[i] != ' ') j = i;
         }


      cfil[j+1] = '\0';
                                        /*  mail the file             */
      if (*stype == 1)
         sprintf (cmd,
            "(echo \"Subject: aips_gripe\" ; cat %s ) | mail %s",
            cfil, addr1);
      else
         sprintf (cmd, "mail -s \"aips_gripe\" %s < %s",
            addr1, cfil);
                                        /* Issue the shell command.  */
      errno = 0;
      if ((zptr = popen (cmd, "r")) == NULL ) {
         *ierr = errno;
         sprintf (msgbuf, "ZGMAI2: Cannot pipe command %s\n", cmd);
         zmsgwr_ (msgbuf, &n7);
         }
      else {
         while (fgets(outbuf, sizeof(outbuf), zptr) != NULL) {
            j = strlen(outbuf);
            if (j > 64) j = 64;
            strncpy (msgbuf, outbuf, j);
            msgbuf[j] = '\0';
            zmsgwr_ (msgbuf, &n3);
            j = strlen(outbuf) - 64;
            if (j > 0) {
               strncpy (msgbuf, &outbuf[64], j);
               msgbuf[j] = '\0';
               zmsgwr_ (msgbuf, &n3);
               }
            }
         *ierr = pclose(zptr);
                                        /* AIX returns ECHILD here    */
                                        /* because of the "sleep".    */
         if ((*ierr != 0) && (errno != ECHILD)) {
            *ierr = errno;
            perror("ZGMAI2");
            sprintf (msgbuf, "ZGMAI2: error %d from pclose()",
               *ierr);
            zmsgwr_ (msgbuf, &n7);
            }
                                         /* delete the file           */
         else {
            *ierr = errno = 0;
            if (unlink (cfil) != 0) *ierr = errno;
            }
         }
      }

   return;
}
