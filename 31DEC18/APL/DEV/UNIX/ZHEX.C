#define Z_hex__
#include <stdio.h>
#include <ctype.h>

#if __STDC__
   void zhex_(int *ival, int *nc, char *hval)
#else
   void zhex_(ival, nc, hval)
   int *ival, *nc;
   char hval[];
#endif
/*--------------------------------------------------------------------*/
/*! encode an integer into hexadecimal characters                     */
/*# Service                                                           */
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
/*  Convert decimal to nc character hexidecimal 'string' - leading    */
/*  blanks are made into 0's, i.e. "illegal" Fortran format Znc.nc    */
/*  Input:                                                            */
/*     ival   I       Decimal value to convert                        */
/*     nc     I       Width of receiving field                        */
/*  Output:                                                           */
/*     hval   C*(*)   String receiving hexidecimal conversion         */
/*  Generic UNIX version                                              */
/*--------------------------------------------------------------------*/
{
   register int i;
   char temp[17];
/*--------------------------------------------------------------------*/

   sprintf(temp,"%*x",*nc,*ival);

   for (i = 0; i < *nc && temp[i] != '\0'; i++)
      if (islower(temp[i]))
         hval[i] = toupper(temp[i]);
      else if (temp[i] == ' ')
         hval[i] = '0';
      else
         hval[i] = temp[i];

   for (i = i; i < *nc; i++)
      hval[i] = ' ' ;

   return;
}
