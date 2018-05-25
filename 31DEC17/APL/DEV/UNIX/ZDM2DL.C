#define Z_dm2dl__
#include <stdlib.h>
#include <stdio.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zdm2dl_(int *nwords, double *inbuf, double *outbuf)
#else
   void zdm2dl_(nwords, inbuf, outbuf)
   int *nwords;
   double *inbuf, *outbuf;
#endif
/*--------------------------------------------------------------------*/
/*! convert ModComp R*6(padded) and R*8 into local double precision   */
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
/*  Convert Modcomp R*6 (zero padded to R*8) or R*8 data into local   */
/*  double precision                                                  */
/*  Inputs:                                                           */
/*     nwords   I        Length of the input buffer in words          */
/*     inbuf    R*8(*)   Input buffer containing Modcomp R*6          */
/*                          (zero padded to R*8) or R*8 data          */
/*  Output:                                                           */
/*     outbuf   R*8(*)   Output buffer containing local R*8 data      */
/*  Notes:                                                            */
/*  (1) The INBUF should have its bytes flipped by a call to ZBYTFL   */
/*      which will leave the values split between 2 local integers.   */
/*      Do not call ZI32IL which may swith 16-bit words, or ZI16Il    */
/*      which will expand each 16-bit part to 32-bits.                */
/*  (2) First 32 bits:                                                */
/*        Expects, after word flip on VMS only, a sign bit in bit 31  */
/*        (1=>negative), bits 22:30 are the  exponent biased by 512,  */
/*        bits 0:21 are the normalized fraction.  Negative values are */
/*        obtained by 2's complement of the whole word.               */
/*      Second 32 bits:                                               */
/*        Just extended precision bits.                               */
/*  (3) Should work inplace.                                          */
/*  Generic UNIX version (currently only handles IEEE and VAX G       */
/*  floating-point formats and where Z_nwdpdp is 2).                  */
/*--------------------------------------------------------------------*/
{
                                        /* Host floating-point format */
   extern int Z_nwdpdp, Z_dpfrmt, Z_bytflp;
                                        /* Local variables.           */
   double *pin, *pout;
   int test, n10 = 10;
   short int sitemp;
   unsigned int sign, exponent, mantissa, temp, templo, bits, xsign;
   register int i;
   char msgbuf[80];
   union u_tag {
      double r8;
      unsigned u4[2];
      short int u2[4];
   } what;
/*--------------------------------------------------------------------*/
                                        /* Will this routine work     */
                                        /* for host floating point    */
                                        /* formats and relative       */
                                        /* sizes?                     */
   if ((Z_nwdpdp == 2) && (Z_dpfrmt == 1 || Z_dpfrmt == 3)) {
      pin = inbuf;
      pout = outbuf;
                                        /* Conversion loop.           */
      for (i = 0; i < *nwords; i++) {
                                        /* Get Modcomp value.         */
         what.r8 = *pin++;
                                        /* swap words if needed       */
         if (Z_bytflp > 1) {
            sitemp = what.u2[0];
            what.u2[0] = what.u2[1];
            what.u2[1] = sitemp;
            sitemp = what.u2[2];
            what.u2[2] = what.u2[3];
            what.u2[3] = sitemp;
            }
                                        /* Get hi as unsigned int.    */
         temp = what.u4[0];
                                        /* Mask out sign bit.         */
         xsign = 0x80000000;
         sign = xsign & temp;
                                        /* If negative, 2's           */
                                        /* complement the whole word. */
         if (sign != 0) {
            templo = what.u4[1];
            what.u4[1] = ~templo + 1;
            temp = (~temp);
                                        /* If msb of lo word are      */
                                        /* unchanged, add 1 to high   */
                                        /* word.                      */
            if ((templo & 0x80000000) == (what.u4[1] & 0x80000000))
               temp = temp + 1;
            }
                                        /* Correct for exponent bias  */
                                        /* and trap for 0.            */
         switch (Z_dpfrmt) {

            case 1:
                                        /* IEEE (bias = 766?).        */
               test = ((0x7fc00000 & temp) >> 22) + 766;
               if (test == 766) test = 0;
               break;

            case 3:
                                        /* VAX G (bias = 768).        */
               test = ((0x7fc00000 & temp) >> 22) + 768;
               if (test == 768) test = 0;
               break;
            }

         exponent = test << 20;
         mantissa = (0x001fffff & temp);
                                        /* Move lsb to next word.     */
         bits = (mantissa & 0x1) << 31;
                                        /* Shift high mantissa.       */
         mantissa = mantissa >> 1;
                                        /* Shift low mantissa.        */
         what.u4[1] = what.u4[1] >> 1;
                                        /* Lsb from hi word.          */
         what.u4[1] = what.u4[1] | bits;
         what.u4[0] = sign | exponent | mantissa;
                                        /* swap words if needed       */
         if (Z_bytflp > 1) {
            sitemp = what.u2[0];
            what.u2[0] = what.u2[2];
            what.u2[2] = sitemp;
            sitemp = what.u2[1];
            what.u2[1] = what.u2[3];
            what.u2[3] = sitemp;
            }
                                        /* Store result.              */
         *pout++ = what.r8;
         }
      }
   else {
                                        /* This routine won't work.   */
      sprintf (msgbuf,
         "ZDM2DL: THIS ROUTINE REQUIRES LOCAL DEVELOPMENT");
      zmsgwr_ (msgbuf, &n10);
      abort ();
      }
                                     /* Exit.                      */
   return;
}
