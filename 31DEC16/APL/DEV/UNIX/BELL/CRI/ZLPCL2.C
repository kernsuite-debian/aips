#include <stdio.h>
#include <errno.h>

#define MAXPATH 128                     /* Maximum path name length.  */

zlpcl2_(llen, lprfil, delfil, prtmax, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! queue a file to the line printer and delete                       */
/*# Z2 Printer                                                        */
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
/*  Queue a line printer file to a printer, then delete it            */
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
/*  UNICOS version  - invokes a shell script called ZLPCL2 which      */
/*  may require local development.                                    */
/*--------------------------------------------------------------------*/
int *llen, *delfil, *prtmax, *syserr, *ierr;
char lprfil[MAXPATH];
/*--------------------------------------------------------------------*/
{
   extern int errno;
   int n7 = 7, n3 = 3;
   register int i;
   char cmd[256], pathname[128], msgbuf[80];
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Form null terminated path  */
                                        /* name of the line printer   */
                                        /* file.                      */
   for (i = 0; i < MAXPATH && i < *llen && lprfil[i] != ' '; i++)
      pathname[i] = lprfil[i];
   pathname[i] = '\0';
                                        /* Form shell command using the */
                                        /* script ZLPCL2 actually send  */
                                        /* the file to the line printer.*/
   sprintf(cmd, "ZLPCL2 %s %d", pathname, *prtmax);
                                        /* Issue the shell command.   */
   errno = 0;
   if (ISHELL (cmd) != 0) {
      *ierr = 1;
      *syserr = errno;
      sprintf (msgbuf, "ZLPCL2: ERROR PRINTING FILE");
      zmsgwr_ (msgbuf, &n7);
      }
                                        /* Delete the file.           */
   else if (*delfil > 0) {
      if (unlink (pathname) == -1) {
                                        /* File may not exist if the  */
                                        /* local ZLPCL2 script is     */
                                        /* designed to delete it.     */
         if (errno != ENOENT) {
                                        /* Some other error.          */
            *ierr = 1;
            *syserr = errno;
            sprintf (msgbuf, "ZLPCL2: ERROR DELETING FILE");
            zmsgwr_ (msgbuf, &n7);
            }
         }
      else {
         sprintf (msgbuf, "ZLPCL2: deleted %s", pathname);
         zmsgwr_ (msgbuf, &n3);
         }
      }

   return;
}
