#define Z_rdmf__
#include <stdio.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zrdmf_(int *nwords, unsigned char *inbuf, int *outbuf, int *flag)
#else
   void zrdmf_(nwords, inbuf, outbuf, flag)
   int *nwords, *outbuf, *flag;
   unsigned char *inbuf;
#endif
/*--------------------------------------------------------------------*/
/*! convert DEC (36 bits in 40) magtape format to 2 local integers    */
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
/*  Convert data packed in DEC-Magtape format (DMF) to 2 local        */
/*  32-bit integers                                                   */
/*                                                                    */
/*  The DMF format is:                                                */
/*                                                                    */
/*     Track    1    2    3    4    5    6    7    8                  */
/*     Byte                                                           */
/*       1      F0   F1   F2   F3   F4   F5   F6   F7                 */
/*       2      F8   F9  F10  F11  F12  F13  F14  F15                 */
/*       3      F16  F17  R0   R1   R2   R3   R4   R5                 */
/*       4      R6   R7   R8   R9  R10  R11  R12  R13                 */
/*       5       0    0    0    0  R14  R15  R16  R17                 */
/*                                                                    */
/*  where Rn refers to the right halfword, Fn to the left halfword.   */
/*                                                                    */
/*  Since the purpose of this routine is to read Modcomp tapes        */
/*  written with this peculiar format, F16, F17, R16 and R17 (the     */
/*  high order bits) are zero for VLA data, but are used for the      */
/*  word count.                                                       */
/*                                                                    */
/*  The first word (5 bytes) of a tape block contains the word count  */
/*  of the block.  The word count is a 16-bit twos-complement integer */
/*  comprised of bits R2-R17.  All other words are treated as pairs   */
/*  of 16-bit, twos-complement integers comprising bits F0-F15 and    */
/*  R0-R15.                                                           */
/*                                                                    */
/*  Input data is assumed to be packed into 1 1/4 integers and output */
/*  data will be returned in a pair of local integers per DEC-10      */
/*  word.  The first integer of each pair corresponds to the left     */
/*  DEC-10 halfword (zero for the word count) and the second to the   */
/*  right halfword.  The sign of each half-word is preserved on       */
/*  expansion to a local integer.                                     */
/*                                                                    */
/*  Inputs:                                                           */
/*     nwords   I      Length of the input buffer in DEC-10 words     */
/*     inbuf    I(*)   Input buffer containing DMF format data        */
/*     flag     I      If > 0, the first word word is the beginning of*/
/*                     a tape block                                   */
/*  Output:                                                           */
/*     outbuf   I(*)   Output buffer containing two local integers per*/
/*                     input DEC-10 word                              */
/*  Generic UNIX version - only for 32-bit twos-complement local      */
/*  integers; have to live with this since the tape contains mixed    */
/*  binary data).                                                     */
/*--------------------------------------------------------------------*/
{
                                        /* # of bits per word         */
   extern int Z_nbitwd;
   int loop, n, *pout, n8 = 8;
   unsigned char *pin;
   char msgbuf[80];
   union u_tag {
      unsigned char bword[4];
      int lword;
   } what;
/*--------------------------------------------------------------------*/
                                        /* This version only good for */
                                        /* 32-bit local integers.     */
   if (Z_nbitwd != 32) {
      sprintf (msgbuf,
         "ZRDMF: THIS VERSION ONLY GOOD FOR 32-BIT LOCAL INTEGERS");
      zmsgwr_ (msgbuf, &n8);
      }
   else {
                                        /* Initialize loop counter    */
                                        /* pointers.                  */
      n = *nwords;
      pin = inbuf;
      pout = outbuf;
                                        /* Special treatment for      */
                                        /* first word in tape block   */
      if (*flag > 0) {
	                                /* First half                 */
	 pin +=2;
	 *pout++ = 0;
	                                /* Second half                */
	 what.bword[2] = *pin++ << 4;
	 what.bword[2] |= *pin >> 4;
	 what.bword[3] = *pin++ << 4;
	 what.bword[3] |= *pin++;
	                                /* Sign extension             */
	 if (what.bword[2] & 0x80)
	    what.bword[0] = what.bword[1] = 0xff;
	 else
	    what.bword[0] = what.bword[1] = 0x00;
	 *pout++ = what.lword;
	 n--;
         }
                                        /* Loop over values.          */
      for (loop = 0; loop < n; loop++) {
                                        /* First half.                */
         what.bword[2] = *pin++;
         what.bword[3] = *pin++;
                                        /* Sign extension             */
	 if (what.bword[2] & 0x80)
	    what.bword[0] = what.bword[1] = 0xff;
	 else
	    what.bword[0] = what.bword[1] = 0x00;
         *pout++ = what.lword;
                                        /* Second half.               */
         what.bword[2] = *pin++ << 2;
	 what.bword[2] |= *pin >> 6;
         what.bword[3] = *pin++ << 2;
	 what.bword[3] |= *pin++ >> 2;
         if (what.bword[2] & 0x80)
	    what.bword[0] = what.bword[1] = 0xff;
	 else
	    what.bword[0] = what.bword[1] = 0x00;
         *pout++ = what.lword;
         }
      }
                                        /* Exit.                      */
   return;
}
