#define Z_sssc2__
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <AIPSPROTO.H>
#define PACKSIZE (65536+6)

#if __STDC__
   void zsssc2_(int *fcb, int *ierr)
#else
   void zsssc2_(fcb, ierr)
   int *fcb, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! Close SUN Screen Server device, flushing any buffer               */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997, 2008                                    */
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
/*  deassigns via 'close' certain devices here the SSS TV socket      */
/*  If buffered, write the last buffer first                          */
/*  Inputs:                                                           */
/*     fcb    I(*)   file control block of open socket                */
/*  Output:                                                           */
/*     ierr   I      error code: 0 => okay                            */
/*                      1 => error in close                           */
/*                      2 => error in flush buffer                    */
/*--------------------------------------------------------------------*/
{
   extern int buffered;
   extern short int packet_int[PACKSIZE];
   extern int pack_offset;
   int size_i2 = sizeof(short int);
   int n7 = 7;
   char msg[80];
/*--------------------------------------------------------------------*/
   *ierr = 0;
   if ((buffered) && (pack_offset > 0)) {
      if (write((*fcb), (char *)packet_int, pack_offset*size_i2) <
         (pack_offset*size_i2)) {
         sprintf (msg, "zsssx2: write packet %s", strerror(errno));
         zmsgwr_ (msg, &n7);
         *ierr = 2;
         }
      pack_offset = 0;
      }

   if (close(*fcb) != 0)
      *ierr = 1;

   return;
}
