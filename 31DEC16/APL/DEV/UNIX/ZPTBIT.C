#define Z_ptbit__

#if __STDC__
   void zptbit_(int *nbits, int *word, int *bits)
#else
   void zptbit_(nbits, word, bits)
   int *nbits, *word, *bits;
#endif
/*--------------------------------------------------------------------*/
/*! build integer word from array of bits                             */
/*# Binary                                                            */
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
/*  Build "word" from "nbits" bit values contained in the array       */
/*  "bits", where bits[0] supplies the lsb, bits[1] the next higher   */
/*  bit, etc.  The rest of the bits in "word" are set to zero.  For   */
/*  example, if                                                       */
/*                                                                    */
/*  bits[0:*] = 0 1 1 1 0 1 0 1 ... 0 1 1 1 0 0 0 0                   */
/*              ^                                 ^                   */
/*             LSB                               MSB                  */
/*  and nbits = 4, then word = 14 (decimal)                           */
/*                                                                    */
/*  Inputs:                                                           */
/*     nbits   I      Number of bits to use from the array "bits"     */
/*     bits    I(*)   Array of bit values (0 or 1)                    */
/*  Output:                                                           */
/*     word    I      Result containing bit pattern from the first    */
/*                    "nbits" values of the array "bits"              */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   register int i;
   int *mybits;
/*--------------------------------------------------------------------*/

   *word = 0;
   mybits = bits;

   mybits += (*nbits - 1);

   for (i = 0; i < *nbits; i++) {
      *word <<= 1;
      *word |= *mybits;
      --mybits;
      }

   return;
}
