#define Z_crlog__
#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#define MAXL 65                         /* Max. "lognam" length.      */
#define MAXT 257                        /* Max. "tlog"   length.      */
#define MAXE 1000                       /* Max environment size       */

#if __STDC__
   void zcrlog_(int *llen, char lognam[], int *tlen, char tlog[],
      int *ierr)
#else
   void zcrlog_(llen, lognam, tlen, tlog, ierr)
   int *llen, *tlen, *ierr;
   char lognam[], tlog[];
#endif
/*--------------------------------------------------------------------*/
/*! (re)defiine a logical name                                        */
/*# Z System                                                          */
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
/*  Defines a new logical name (i.e., environment variable) or        */
/*  redefines an old one.                                             */
/*  Inputs:                                                           */
/*     llen     I       Length of logical name                        */
/*     lognam   C*(*)   Logical name - ends with  1st colon or blank  */
/*     tlen     I       Length of tlog                                */
/*     tlog     C*(*)   Translation for logical name:                 */
/*                      ends on last non-blank or tlen                */
/*  Outputs:                                                          */
/*     ierr     I       Error return code: 0 => okay                  */
/*                         1 => error                                 */
/*  General UNIX version: makes its own envitronment to be certain    */
/*  that it is large enough.  System provided ones often had no room  */
/*  for new names!                                                    */
/*--------------------------------------------------------------------*/
{
                                        /* UNIX environment           */
   extern char **environ;
                                        /* For expanding environ, if  */
                                        /* necessary.                 */
   char *ptr;

   int found, need, nlen;
   char lname[MAXL], tname[MAXT], envnew[MAXT], envold[MAXT];
   register int i, j;
   static int zcrlogin = 1;
   static char *myenviron[MAXE+1];
/*--------------------------------------------------------------------*/
   *ierr = 0;
   if (*llen < 1) return;
                                        /* point environ to big area  */
   if (environ != myenviron) {
      zcrlogin = 0;
      for (i = 0; i < MAXE &&  environ[i] != NULL; i++)
         myenviron[i] = environ[i];
      if (i < MAXE) {
         environ = myenviron;
         myenviron[i] = NULL;
         }
      }
                                        /* Form null terminated       */
                                        /* logical name (i.e.,        */
                                        /* environment variable).     */
                                        /* Terminate on ' ' or ':'    */
   for (i = 0; i < *llen && i < MAXL && lognam[i] != ' ' &&
      lognam[i] != ':'; i++)
      lname[i] = lognam[i];
   lname[i] = '\0';
                                        /* Form null terminated       */
                                        /* definition.                */
   j = 0;
   for (i = 0; i < *tlen && i < MAXT && tlog[i] != '\0'; i++) {
      tname[i] = tlog[i];
      if (tname[i] != ' ') j = i;
      }
   tname[j+1] = '\0';
                                        /* Cast into environment      */
                                        /* definition.                */
   sprintf(envnew,"%s=%s",lname,tname);
   nlen = strlen(lname) + 1;
                                        /* Search for extant          */
                                        /* definition.                */
   found = 0;
   for (i = 0; environ[i] != NULL; i++) {
                                        /* Re-define, expanding       */
                                        /* "environ[i]" if necessary. */
      if (strncmp(envnew,environ[i],nlen) == 0) {
         found = 1;
/*         sprintf(envold,"%s=%s",lname,getenv(lname)); */
         need = strlen(envnew) - strlen(environ[i]);
         if (need > 0) {
            need = strlen(envnew) + 1;
            if ((ptr = malloc(need)) == NULL) {
               *ierr = 1;
               goto exit;
               }
            environ[i] = ptr;
            }
         strcpy (environ[i], envnew);
         }
      }
                                        /* If not found, then need to */
                                        /* expand environ[i] and      */
                                        /* add definition.            */
   if (found == 0 && *tlen >= 0) {
      if (i >= MAXE) {
         *ierr = 2;
         }
      else {
         need = strlen(envnew) + 1;
         if ((environ[i] = malloc(need)) == NULL) {
            *ierr = 1;
            }
         else {
            strcpy (environ[i], envnew);
                                        /* Important!                 */
            environ[i+1] = NULL;
            }
         }
      }

exit:

   return;
}
