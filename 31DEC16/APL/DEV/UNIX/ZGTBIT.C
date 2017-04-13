#define Z_gtbit__
#define MASK 01

#if __STDC__
   void zgtbit_(int *nbits, int *word, int *bits)
#else
   void zgtbit_(nbits, word, bits)
   int *nbits, *word, *bits;
#endif
/*--------------------------------------------------------------------*/
/*! get array of bits from a word                                     */
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
/*  Get the lowest order "nbits" bits of the bit pattern in "word"    */
/*  and return them in the array BITS with the lsb in bits[0].  For   */
/*  example, if                                                       */
/*                                                                    */
/*  word  = 0 0 0 0 0 1 0 1 ... 0 0 0 1 0 0 1 1                       */
/*          ^                                 ^                       */
/*         MSB                               LSB                      */
/*  and nbits = 3 then bits[0] = 1 , bits[1] = 1 and bits[2] = 0      */
/*                                                                    */
/*  Inputs:                                                           */
/*     nbits   I      Number of bits                                  */
/*     word    I      Word from which to extract bits                 */
/*  Output:                                                           */
/*     bits    I(*)   Bit array (values 0 or 1)                       */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   register int i ;
   int myword, *mybits ;
/*--------------------------------------------------------------------*/

   myword = *word;
   mybits = bits;

   for (i = 0; i < *nbits; ++i) {
      *mybits = myword & MASK;
      ++mybits;
      myword = myword >> 1;
      }

   return;
}
