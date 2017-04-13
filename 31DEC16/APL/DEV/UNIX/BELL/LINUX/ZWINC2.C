#ifndef _POSIX_SOURCE
#define _POSIX_SOURCE
#endif
#define Z_winc2__
#include <sys/ioctl.h>

#if __STDC__
   void zwinc2_(int *wcols, int *wlines)
#else
   zwinc2_(wcols, wlines)
   int *wcols, *wlines;
#endif
/*--------------------------------------------------------------------*/
/*! detects current window size                                       */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1998-1999                                          */
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
/*  Inputs:                                                           */
/*     none                                                           */
/*  Output:                                                           */
/*     wcols    I     >0 => new size                                  */
/*     wlines   I      0 => failure: fall back to old size            */
/*                    >0 => new size                                  */
/*  Linux version: Jeff Uphoff, NRAO, July 1998, Greisen 7/99         */
/*--------------------------------------------------------------------*/
{
   struct winsize ws;
/*--------------------------------------------------------------------*/
   if (ioctl (1, TIOCGWINSZ, &ws, sizeof (struct winsize)) == 0) {
     *wcols  = ws.ws_col;
     *wlines = ws.ws_row;
   } else {
     *wcols  = 0;               /* Fall back to default on failure.   */
     *wlines = 0;               /* Fall back to default on failure.   */
   }
   return;
}
