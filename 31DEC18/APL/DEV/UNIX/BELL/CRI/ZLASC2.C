#include <stdio.h>
#include <errno.h>

#define MAXPATH 128                     /* Maximum path name length.  */

zlasc2_(flen, filnam, delfil, syserr)
/*--------------------------------------------------------------------*/
/*! queue a close text (print/plot) file to the laser printer         */
/*# Z2 Printer Plot-util                                              */
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
/*  Queue a laser plotter file for printing/plotting                  */
/*  Inputs:                                                           */
/*     flen     I       Length of "filnam" in characters              */
/*     filnam   C*(*)   Full path name of the file to be queued       */
/*     delfil   I       Disposition indicator code:                   */
/*                         0 => keep after printing                   */
/*                         >0 => delete after delfil seconds          */
/*  Output:                                                           */
/*     syserr   I          System error code (0 okay)                 */
/*  UNICOS version (invokes a shell script called ZLASCL which        */
/*  may require local development).                                   */
/*--------------------------------------------------------------------*/
char filnam[MAXPATH];
int *flen, *delfil, *syserr;
/*--------------------------------------------------------------------*/
{
   extern int errno;
   int n6 = 6;
   register int i;
   char cmd[256], pathname[MAXPATH], msgbuf[80];
/*--------------------------------------------------------------------*/
                                        /* Form null terminated path  */
                                        /* name.                      */
   for (i = 0; i < MAXPATH && i < *flen && filnam[i] != ' '; i++)
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
      errno = 0;
      if (ISHELL (cmd) != 0) {
         *syserr = errno;
         sprintf (msgbuf, "ZLASCL: ERROR PRINTING FILE");
         zmsgwr_ (msgbuf, &n6);
         }
                                        /* Delete the file.           */
      else if (*delfil > 0) {
         if (unlink (pathname) == -1) {
                                        /* File may not exist if the  */
                                        /* local ZLASCL script is     */
                                        /* designed to delete it.     */
            if (errno != ENOENT) {
                                        /* Some other error.          */
               *syserr = errno;
               sprintf (msgbuf, "ZLASCL: ERROR DELETING FILE");
               zmsgwr_ (msgbuf, &n6);
               }
            }
         else {
            sprintf (msgbuf, "ZLASCL: deleted %s", pathname);
            zmsgwr_ (msgbuf, &n6);
	    }
         }
      }

   return;
}
