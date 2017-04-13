#define Z_path__
#include <stdio.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zpath_(int *llen, char *logfil, int *flen, char *fulnam,
      int *fact, int *ierr)
#else
   void zpath_(llen, logfil, flen, fulnam, fact, ierr)
   int *llen, *flen, *fact, *ierr;
   char logfil[], fulnam[];
#endif
/*--------------------------------------------------------------------*/
/*! convert a file name 'Logical:file' to full path name              */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2000, 2002                             */
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
/*  Translate a file specifcation of the form "logical:file" to a     */
/*  full path name.  "/..." absolute path name is also allowed.       */
/*  Inputs:                                                           */
/*     llen     I          Length of "logfil" (1-relative)            */
/*     logfil   C*(*)      File specification (may contain a valid,   */
/*                         colon terminated logicalvariable)          */
/*     flen     I          Length of "fulnam" (1-relative)            */
/*  Output:                                                           */
/*     fulnam   C*(flen)   Full path name (blank filled)              */
/*     fact     I          Position of last non-blank in "fulnam"     */
/*                         (1-relative)                               */
/*     ierr     I          Error return code: 0 => no error           */
/*                             1 => improper combination of inputs    */
/*                             2 => no translation for "logical"      */
/*                             3 => "fulnam" too short                */
/*                             4 => logical name required             */
/*  Generic UNIX version  - "logical" => environment variable is      */
/*  required by UNIX (IERR = 4 otherwise).                            */
/*--------------------------------------------------------------------*/
{
   char envvar[MAXAPATH+1], xlated[MAXAPATH+1], filename[MAXAPATH+1],
      pathname[MAXAPATH+1];
   int elen, xlen, xlnb, jerr;
   register int i, j;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Extract logical.           */
   for (i = 0; logfil[i] != ':' && i < *llen && i < MAXAPATH; i++)
      envvar[i] = logfil[i];
   envvar[i] = '\0';
   if (logfil[i] != ':') {
      elen = 0;
      i = -1;
      }
   else
      elen = i;
                                        /* Extract filename.          */
   j = 0;
   for (i = i+1; i < *llen && logfil[i] != ' '; i++) {
      filename[j] = logfil[i];
      j++;
      }
   filename[j] = '\0';
                                        /* Check for no file name     */
   if (j == 0)
     *ierr = 4;
                                        /* no logical                 */
   else if (elen == 0) {
                                        /* Copy full path name to     */
                                        /* "fulnam".                  */
      for (i = 0; i < *flen && filename[i] != '\0'; i++)
         fulnam[i] = filename[i];
                                        /* Record actual full file    */
                                        /* name length in characters  */
                                        /* (1-relative).              */
      *fact = i;
                                        /* Fill remainder of "fulnam" */
                                        /* with blanks.               */
      for (i = i; i < *flen; i++)
         fulnam[i] = ' ';
      }
                                        /* Translate logical and null */
                                        /* terminate.                 */
   else {
      xlen = MAXAPATH + 1;
      ztrlo2_ (&elen, envvar, &xlen, xlated, &xlnb, &jerr);
      if (jerr != 0) {
         *ierr = 2;
         }
      else {
         xlated[xlnb] = '\0';
                                        /* Build full path name.      */
         sprintf (pathname, "%s/%s", xlated, filename);
                                        /* Copy full path name to     */
                                        /* "fulnam".                  */
         for (i = 0; i < *flen && pathname[i] != '\0'; i++)
            fulnam[i] = pathname[i];
                                        /* Record actual full file    */
                                        /* name length in characters  */
                                        /* (1-relative).              */
         *fact = i;
                                        /* Fill remainder of "fulnam" */
                                        /* with blanks.               */
         for (i = i; i < *flen; i++)
            fulnam[i] = ' ';
         }
      }

   return;
}
