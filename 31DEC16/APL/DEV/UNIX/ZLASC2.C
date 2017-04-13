#define Z_lasc2__
#define _XOPEN_SOURCE                      /* Needed for popen/pclose */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <AIPSPROTO.H>

#define ZL2_DELAY 10                        /* delay min 10 sec       */

#if __STDC__
   void zlasc2_(int *flen, char filnam[MAXAPATH], int *delfil,
      int *syserr)
#else
   void zlasc2_(flen, filnam, delfil, syserr)
   char filnam[MAXAPATH];
   int *flen, *delfil, *syserr;
#endif
/*--------------------------------------------------------------------*/
/*! spool a graphics file to the appropriate printer and delete it    */
/*# Z2 Printer Plot-util                                              */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997                                          */
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
/*  Spool a laser plotter file for printing/plotting, then delete it  */
/*  Inputs:                                                           */
/*     flen     I       Length of "filnam" in characters              */
/*     filnam   C*(*)   Full path name of the file to be queued       */
/*     delfil   I       Disposition indicator code:                   */
/*                         0 => keep after printing                   */
/*                         1 => delete after printing                 */
/*  Output:                                                           */
/*     syserr   I          System error code (0 okay)                 */
/*  Generic UNIX version (invokes a shell script called ZLASCL)       */
/*                                                                    */
/*  The delete-after-printing option will shed a process to delete    */
/*  the file after a small delay.  If the print command in the ZLASCL */
/*  shell script causes a symbolic link to be made to the file, and   */
/*  the spooler is backed up, it is possible that the file will be    */
/*  deleted here before being printed.  Not all spooling programs     */
/*  (e.g. lp on System V) have an option for deleting after printing. */
/*--------------------------------------------------------------------*/
{
   int n7 = 7, n3 = 3, zlpid, idel;
   register int i, j;
   char cmd[256], pathname[MAXAPATH], msgbuf[80], outbuf[132];
   FILE *zptr;
/*--------------------------------------------------------------------*/
   *syserr = 0;
                                        /* Form null terminated path  */
                                        /* name.                      */
   for (i = 0; i < MAXAPATH && i < *flen && filnam[i] != ' '; i++)
      pathname[i] = filnam[i];
   pathname[i] = '\0';
                                        /* Does file exist?           */
   if (access (pathname, 0) == -1) {
      *syserr = errno;
      }
                                        /* Use shell script ZLASCL to */
                                        /* do the actual printing.    */
   else {
      sprintf (cmd, "ZLASCL %s", pathname);
                                        /* Issue the shell command.   */
      errno = 0;
      if ((zptr = popen (cmd, "r")) == NULL ) {
         *syserr = errno;
         sprintf (msgbuf, "ZLASC2: Cannot pipe command %s\n", cmd);
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
         *syserr = pclose(zptr);
                                        /* AIX returns ECHILD here    */
                                        /* because of the "sleep".    */
         if ((*syserr != 0) && (errno != ECHILD)) {
            *syserr = errno;
            perror("ZLASC2");
            sprintf (msgbuf, "ZLASC2: ignoring error %d from pclose()",
                     *syserr);
            zmsgwr_ (msgbuf, &n3);
            }
         *syserr = errno = 0;
         }
                                        /* Delete the file.           */
      if ((*delfil > 0) && (*syserr == 0)) {
         idel = ZL2_DELAY;
         if (*delfil > idel) idel = *delfil;
         sprintf (cmd,
            "((sleep %d; rm -f %s\n)&) >/dev/null 2>/dev/null",
            idel, pathname);
         if ((zptr = popen (cmd, "r")) == NULL ) {
            *syserr = errno;
            sprintf (msgbuf, "ZLASC2: Will NOT delete file %s\n", cmd);
            zmsgwr_ (msgbuf, &n7);
            sprintf (msgbuf, "ZLASC2: (because 'popen' failed)\n");
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
            *syserr = pclose(zptr);
                                        /* AIX returns ECHILD here    */
                                        /* because of the "sleep".    */
            if ((*syserr != 0) && (errno != ECHILD)) {
               *syserr = errno;
               perror("ZLASC2");
               sprintf (msgbuf,
                        "ZLASC2: ignoring error %d from 2nd pclose()",
                        *syserr);
               zmsgwr_ (msgbuf, &n3);
               }
            *syserr = errno = 0;
                                        /* give a message     */
            sprintf (msgbuf,
                     "a child process will delete %s in %d seconds",
                     pathname, idel);
            zmsgwr_ (msgbuf, &n3);
            }
         }
      }
   return;
}
