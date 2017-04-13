#define Z_rm2rl__
#include <stdio.h>
#include <stdlib.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zrm2rl_(int *nwords, float *inbuf, float *outbuf)
#else
   void zrm2rl_(nwords, inbuf, outbuf)
   int *nwords;
   float *inbuf, *outbuf;
#endif
/*--------------------------------------------------------------------*/
/*! convert ModComp single precision floating to local                */
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
/*  Convert Modcomp single precision floating point data into local   */
/*  single precision floating point.                                  */
/*  Inputs:                                                           */
/*     nwords   I   Length of the input buffer in words               */
/*     inbuf    R   Input buffer containing Modcomp R*4 data          */
/*  Output:                                                           */
/*     outbuf   R   Output buffer containing local REAL data          */
/*                                                                    */
/*  Notes:                                                            */
/*                                                                    */
/*     Before call, input buffer should have its bytes flipped via    */
/*     ZI32IL which will leave the values in one 32-bit integer       */
/*                                                                    */
/*     Expects, after word flip, sign bit in bit 31 (1=>negative),    */
/*     bits 22:30 are the  exponent biased by 256(?), bits 0:21 are   */
/*     the normalized fraction.  Negative values are obtained by 2's  */
/*     complement of the whole word.                                  */
/*                                                                    */
/*     Should work inplace.                                           */
/*                                                                    */
/*  Generic UNIX version (currently only handles IEEE and VAX F       */
/*  floating-point formats).                                          */
/*--------------------------------------------------------------------*/
{
                                        /* Host single precision      */
                                        /* floating-point format      */
   extern int Z_spfrmt, Z_bytflp;
   float *pin, *pout;
   int test, n10 = 10;
   unsigned sign, exponent, mantissa, temp;
   register int i;
   short int sitemp;
   char msgbuf[80];
   union u_tag {
      float r4;
      unsigned u4;
      short int u2[2];
   } what;
/*--------------------------------------------------------------------*/
                                        /* routine works IEEE, VAX F  */
   if (Z_spfrmt == 1 || Z_spfrmt == 2) {
      pin = inbuf;
      pout = outbuf;
                                        /* Conversion loop.           */
      for (i = 0; i < *nwords; i++) {
                                        /* Get Modcomp value.         */
         what.r4 = *pin++;
                                        /* swap words if needed       */
         if (Z_bytflp > 1) {
            sitemp = what.u2[0];
            what.u2[0] = what.u2[1];
            what.u2[1] = sitemp;
            }
                                        /* Get as unsigned int.       */
         temp = what.u4;
                                        /* Mask out sign bit.         */
         sign = 0x80000000 & temp;
                                        /* If negative, 2's           */
                                        /* complement the whole word. */
         if (sign == 0x80000000) temp = (~temp) + 1;
                                        /* Correct for exponent bias. */
         switch (Z_spfrmt) {
                                        /* IEEE (bias = -130?).       */
            case 1:
               test = ((0x7fc00000 & temp) >> 22) - 130;
               break;
                                        /* VAX F (bias = -128).       */
            case 2:
               test = ((0x7fc00000 & temp) >> 22) - 128;
               break;
            }

         exponent = test << 23;
         mantissa = (0x001fffff & temp) << 2;
         what.u4 = sign | exponent | mantissa;
                                        /* Overflow.                  */
         if (test > 255)
            what.u4 = ~0x0;
                                        /* Underflow.                 */
         else if (test < 1)
            what.u4 = 0;
                                        /* Store result.              */
         *pout++ = what.r4;
         }
      }
                                        /* This routine won't work.   */
   else {
      sprintf (msgbuf,
         "ZRM2RL: THIS ROUTINE REQUIRES LOCAL DEVELOPMENT");
      zmsgwr_ (msgbuf, &n10);
      abort ();
      }

   return;
}
