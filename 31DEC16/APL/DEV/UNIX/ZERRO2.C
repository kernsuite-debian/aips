#define Z_erro2__
#include <stdio.h>
#include <string.h>

#if __STDC__
   void zerro2_(int *syserr, int *elen, char *errmsg)
#else
   void zerro2_(syserr, elen, errmsg)
   int *syserr, *elen;
   char errmsg[];
#endif
/*--------------------------------------------------------------------*/
/*! return system error message for given system error code           */
/*# Service                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997-1998, 2003                              */
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
/*  This routine gets the text of the system error message            */
/*  corresponding to a system error code.  Called by ZERROR.          */
/*  Inputs:                                                           */
/*     syserr   I         System error code                           */
/*  In/Out:                                                           */
/*     elen     I         In:  length of errmsg variable in bytes     */
/*                        Out: 1-relative position of last non-blank  */
/*                             character in error message text        */
/*                             corresponding to syserr (if any,       */
/*                             otherwise, zero)                       */
/*     errmsg   C*(elen)  Text of system error message                */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   register int i;
   int elnb;
/*--------------------------------------------------------------------*/
                                        /* Get system error message   */
   if (*syserr >= 0) {
      sprintf (errmsg, "ERRNO = %d (%s)", *syserr, strerror(*syserr));
      for (i = 0; i < *elen && errmsg[i] != '\0'; i++);
                                        /* Blank fill remainder       */
      elnb = i;
      for (i = elnb; i < *elen; i++)
         errmsg[i] = ' ';
      *elen = elnb;
      }
                                        /* no message in list         */
   else
      *elen = 0;

   return;
}
