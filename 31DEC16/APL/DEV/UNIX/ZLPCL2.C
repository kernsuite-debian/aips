#define Z_lpcl2__
#define _XOPEN_SOURCE                      /* Needed for popen/pclose */
#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <AIPSPROTO.H>

#define ZL2_DELAY 10                        /* delay min 10 seconds   */

#if __STDC__
   void zlpcl2_(int *llen, char lprfil[MAXAPATH], int *delfil,
      int *prtmax, int *syserr, int *ierr)
#else
   void zlpcl2_(llen, lprfil, delfil, prtmax, syserr, ierr)
   int *llen, *delfil, *prtmax, *syserr, *ierr;
   char lprfil[MAXAPATH];
#endif
/*--------------------------------------------------------------------*/
/*! spool a text file to the appropriate printer and delete it        */
/*# Z2 Printer                                                        */
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
/*  Spool a line printer file to a printer, then delete it            */
/*  Inputs:                                                           */
/*     llen     I       Length of "lprfil" in characters              */
/*     lprfil   C*(*)   Name of the line printer file to queue        */
/*     delfil   I       Disposition indicator code:                   */
/*                         0 => keep after printing                   */
/*                         >0 => delete after delfil seconds          */
/*     prtmax   I       Length of a line printer page                 */
/*  Output:                                                           */
/*     syserr   I       System error code                             */
/*     ierr     I       Error return code: 0 => no error              */
/*                         1 => error                                 */
/*  Generic UNIX version  - invokes a shell script called ZLPCL2      */
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
   *ierr = 0;
                                        /* Form null terminated path  */
                                        /* name.                      */
   for (i = 0; i < MAXAPATH && i < *llen && lprfil[i] != ' '; i++)
      pathname[i] = lprfil[i];
   pathname[i] = '\0';
                                        /* Does file exist?           */
   if (access (pathname, 0) == -1) {
      *syserr = errno;
      *ierr = 1;
      }
                                        /* Use shell script ZLPCL2 to */
                                        /* do the actual printing.    */
   else {
      sprintf(cmd, "ZLPCL2 %s %d", pathname, *prtmax);
                                        /* Issue the shell command.   */
      errno = 0;
      if ((zptr = popen (cmd, "r")) == NULL ) {
         *ierr = 1;
         *syserr = errno;
         sprintf (msgbuf, "ZLPCL2: Cannot pipe command %s", cmd);
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
            perror("ZLPCL2");
            sprintf (msgbuf, "ZLPCL2: ignoring error %d from pclose()",
                     *syserr);
            zmsgwr_ (msgbuf, &n3);
            *syserr = 0;
            }
         }
                                        /* Delete the file.           */
      if ((*delfil > 0) && (*ierr == 0)) {
         idel = ZL2_DELAY;
         if (*delfil > idel) idel = *delfil;
         sprintf (cmd,
             "((sleep %d; rm -f %s)&) >/dev/null 2>/dev/null",
             idel, pathname);
         if ((zptr = popen (cmd, "r")) == NULL ) {
            *ierr = 1;
            *syserr = errno;
            sprintf (msgbuf, "ZLPCL2: Will NOT delete file %s", cmd);
            zmsgwr_ (msgbuf, &n7);
            sprintf (msgbuf, "ZLPCL2: (because 'popen' failed)");
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
            *ierr = 0;
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
