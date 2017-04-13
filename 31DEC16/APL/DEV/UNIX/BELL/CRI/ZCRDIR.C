#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#define MAXL 80

zcrdir_(len, name, ierr)
/*--------------------------------------------------------------------*/
/*! create the specified disk directory                               */
/*# IO-basic                                                          */
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
/*  Create the directory specified in the character string "name".    */
/*  Inputs:                                                           */
/*     len    I       string length of name (at least to last char)   */
/*     name   C*(*)   directory name                                  */
/*  Output:                                                           */
/*     ierr   I       Error code:                                     */
/*                       0  directory already existed or creation ok  */
/*                       1  if exists but is not a directory          */
/*                       2  if creation failed                        */
/*  UNICOS version.                                                   */
/*--------------------------------------------------------------------*/
int *ierr, *len ;
char name[] ;
/*--------------------------------------------------------------------*/
{
   char st[100], nam[MAXL];
   int i, mlen, ret;
   struct stat stbuf;
/*--------------------------------------------------------------------*/
   mlen = (*len > MAXL) ? MAXL : *len;
   for (i=0; i < mlen; i++) {
      if (name[i] != ' ')
         nam[i] = name[i];
      else
         nam[i] = '\0';
      }

   ret = stat(nam, &stbuf);
   if (ret == 0) {
      if (stbuf.st_mode & 0040000)
         *ierr = 0;
      else
         *ierr = 1;
      }
   else {
      sprintf(st,"mkdir %s",nam);
      *ierr = ISHELL(st);
      if (*ierr != 0)
         *ierr = 2;
      }

   return;
}
