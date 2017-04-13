#define Z_dicc2__
#include <stdio.h>
#include <stdlib.h>
#include <AIPSPROTO.H>

extern char *dic_filename;              /* Name of Dicomed file       */
extern FILE *dic_file;                  /* Dicomed file stream        */
extern int dic_spool;                   /* Non-zero if file should be */
                                        /* spooled automatically      */
extern int dic_called;                  /* Indicates whether a file   */
                                        /* is open                    */

#if __STDC__
   void zdicc2_(char *fname, int *flen, int *iret)
#else
   void zdicc2_(fname, flen, iret)
   char *fname;
   int *flen, *iret;
#endif
/*--------------------------------------------------------------------*/
/*! Close a Dicomed image file and (optionally) spool it              */
/*# Z                                                                 */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997                                         */
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
/*   Close a Dicomed image file and, if no name was supplied by the   */
/*   user, spool it to the film recorder and delete it.               */
/*                                                                    */
/*   Input:                                                           */
/*      FNAME      C*(*)         Name under which to store file       */
/*      FLEN       I             Number of characters in FNAME; if 0  */
/*                               then file is spooled and deleted.    */
/*                                                                    */
/*   Output:                                                          */
/*      IRET       I             Return status:                       */
/*                                 0 -> success                       */
/*                                                                    */
/*   UNIX version (ignores LUN)                                       */
/*--------------------------------------------------------------------*/
{
    char cmdline[256], file_name[256], msg[80] ;
    int i, n8 = 8;
/*--------------------------------------------------------------------*/

    *iret = 0;
    if (fclose(dic_file) != 0) {
       sprintf (msg, "ZDICCL: ERROR CLOSING FILE\n");
       zmsgwr_ (msg, &n8);
       perror("ZDICCL:");
       *iret = 1;
       return;
       }

   for (i = 0; i < *flen; i++) {
      file_name[i] = fname[i];
      }
   file_name[i] = '\0';
                                        /* The real work is done in   */
                                        /* the ZDICC2 shell script.   */
   sprintf(cmdline, "ZDICC2 %s %s", dic_filename, file_name);
   if (system(cmdline) != 0) {
      sprintf (msg, "ZDICCL: ERROR SPOOLING FILE\n");
      zmsgwr_ (msg, &n8);
      perror("ZIDCCL:");
      *iret = 2;
      }
}
