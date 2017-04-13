#define Z_trlo2__
#include <stdlib.h>
#include <ctype.h>
#include <stdio.h>
#include <AIPSPROTO.H>

#if __STDC__
   void ztrlo2_(int *llen, char *lognam, int *xlen, char *xlated,
      int *xlnb, int *ierr)
#else
   void ztrlo2_(llen, lognam, xlen, xlated, xlnb, ierr)
   int *llen, *xlen, *xlnb, *ierr;
   char lognam[], xlated[];
#endif
/*--------------------------------------------------------------------*/
/*! translate a logical name                                          */
/*# System Z2                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2000                                   */
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
/*  Translate a logical name (i.e., environment variable).            */
/*  NOTE: This routine is ONLY for use by other Z-routines.           */
/*  Inputs:                                                           */
/*     llen     I          Length of "lognam" (1-relative)            */
/*     lognam   H*(llen)   Logical name (must be null, blank or colon */
/*                         terminated)                                */
/*     xlen     I          Length of "xlated" (1-relative)            */
/*  Output:                                                           */
/*     xlated   H*(xlen)   Translation (blank filled)                 */
/*     xlnb     I          Position of last non-blank in "xlated"     */
/*                         (1-relative)                               */
/*     ierr     I          Error return code: 0 => no error           */
/*                             1 => error                             */
/*  Generic UNIX version - uses "getenv"                              */
/*--------------------------------------------------------------------*/
{
   char envvar[MAXPNAME+1], envval[MAXAPATH+1];
   register int i;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Form null terminated       */
                                        /* logical name (i.e.,        */
                                        /* environment variable).     */
                                        /* Terminate on non-          */
                                        /* alphanumeric character.    */
   for (i = 0; i < *llen && i < MAXPNAME && (isalpha (lognam[i]) ||
      isdigit (lognam[i]) || lognam[i] == '_'); i++)
      envvar[i] = lognam[i];
   envvar[i] = '\0';
                                        /* Get value of environment   */
                                        /* variable (if any).         */
   *xlnb = 0;
   envval[0] = '\0';
   if (getenv (envvar) == 0)
      *ierr = 1;
   else {
      sprintf (envval, "%s", getenv (envvar));
      if (envval[0] == '\0')
         *ierr = 1;
      else {
                                        /* Copy translation (up to    */
                                        /* first null) into "xlated". */
         for (i = 0; i < *xlen && i < MAXAPATH && envval[i] != '\0'; i++)
            xlated[i] = envval[i];
                                        /* 1-relative position of     */
                                        /* last non-blank in "xlated".*/
         *xlnb = i;
                                        /* Blank fill remainder of    */
                                        /* "xlated".                  */
         for (i = *xlnb; i < *xlen ; i++)
            xlated[i] = ' ';
         }
      }

   return;
}
