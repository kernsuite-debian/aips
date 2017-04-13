#define Z_bymov__
#if __STDC__
   void zbymov_(int *nmove, int *inp, char inb[], int *outp,
      char outb[])
#else
   void zbymov_(nmove, inp, inb, outp, outb)
   int *nmove, *inp, *outp ;
   char inb[], outb[] ;
#endif
/*--------------------------------------------------------------------*/
/*! move 8-bit bytes from in-buffer to out-buffer                     */
/*# Z Binary                                                          */
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
/*  Moves a string of 8-bit bytes from INB to OUTB.                   */
/*  Inputs:                                                           */
/*     nmove   I      Number of bytes to move                         */
/*     inp     I      First byte (1-relative) in INB to move          */
/*     inb     I(*)   Input buffer                                    */
/*     outp    I      Byte position (1-relative) in OUTB to put       */
/*                          first byte                                */
/*  Output:                                                           */
/*     outb    I(*)   Output buffer                                   */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   register int i, j, k;
/*--------------------------------------------------------------------*/

   for (i = *inp - 1, j = *outp - 1, k = 0; k < *nmove; i++, j++, k++)
      outb[j] = inb[i];

   return;
}
