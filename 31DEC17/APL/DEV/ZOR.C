#define Z_or__

#if __STDC__
   int zor_ (int *a, int *b)
#else
   int zor_ (a, b)
   int *a, *b;
#endif
/*--------------------------------------------------------------------*/
/*! Performs bitwise inclusive OR on corresponding bits               */
/*# Z Service                                                         */
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
/*   Performs bitwise inclusive OR on corresponding bits              */
/*   Inputs:                                                          */
/*      a      I   1st arg                                            */
/*      b      I   2nd arg                                            */
/*   Output:                                                          */
/*      ZOR    I   inclusive OR on bits of a and b                    */
/*   UNIX version - VMS has this built in to Fortran                  */
/*--------------------------------------------------------------------*/
{
   int i;
/*--------------------------------------------------------------------*/

   i = *a | *b ;

   return (i) ;
}
