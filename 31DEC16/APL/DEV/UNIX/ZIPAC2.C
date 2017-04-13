#define Z_ipac2__
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zipac2_(int *nvals, int *code, int *bytflp, int *np,
      signed char sbuf[], signed char lbuf[])
#else
   void zipac2_(nvals, code, bytflp, np, sbuf, lbuf)
   signed char sbuf[], lbuf[];
   int *nvals, *code, *bytflp, *np;
#endif
/*--------------------------------------------------------------------*/
/*! I32 <-> I16 w and w/o byte swap and move between sbuf and lbuf    */
/*# Z2 Binary                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1998-1999, 2005                                    */
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
/*  Change 16-bit integer buffer into 32-bit and vice versa with and  */
/*  without byte swapping.                                            */
/*  Inputs:                                                           */
/*     nvals    I      Number of integers involved                    */
/*     code     I      Operation code                                 */
/*                        0 => S -> L: neither are swapped            */
/*                        1 => S -> L: byte swapped only              */
/*                        2 => S -> L: word swapped only              */
/*                        3 => S -> L: both byte and word swapped     */
/*                        4 => L -> S: neither are swapped            */
/*                        5 => L -> S: byte swapped only              */
/*                        6 => L -> S: word swapped only              */
/*                        7 => L -> S: both byte and word swapped     */
/*     bytflp   I      Local byte flip code                           */
/*  Output:                                                           */
/*     sbuf   I*2(*)   short int buffer                               */
/*     lbuf   I*4(*)   long int buffer                                */
/*                        (may be the same as the input buffer)       */
/* Generic UNIX version                                               */
/*--------------------------------------------------------------------*/
{
   register int j, k, i, m;
   signed char temp[4], sign, plus, minus;
/*--------------------------------------------------------------------*/
   plus = 0x00;
   minus = 0xff;
   if (*code < 4 ) {
      j = 2 * (*np + *nvals - 2);
      k = 4 * (*nvals -1);
      }
   else {
      j = 2 * (*np - 1);
      k = 0;
      }
   m = 0;
   if ((*bytflp == 1) || (*bytflp == 3)) m = 1;
                                        /* no flip: S->L              */
   if (*code == 0) {
      for (i = 0; i < *nvals; i++) {
         lbuf[k+2] = sbuf[j];
         lbuf[k+3] = sbuf[j+1];
         if (sbuf[j+m] < 0)
            lbuf[k] = minus;
         else
            lbuf[k] = plus;
         lbuf[k+1] = lbuf[k];
         j -= 2;
         k -= 4;
         }
      }
                                        /* Swap bytes: S->L.          */
   else if (*code == 1) {
      for (i = 0; i < *nvals; i++) {
         lbuf[k+3] = sbuf[j];
         lbuf[k+2] = sbuf[j+1];
         if (sbuf[j+m] < 0)
            lbuf[k] = minus;
         else
            lbuf[k] = plus;
         lbuf[k+1] = lbuf[k];
         j -= 2;
         k -= 4;
         }
      }
                                        /* Swap words: S->L.          */
   else if (*code == 2) {
      for (i = 0; i < *nvals; i++) {
         lbuf[k] = sbuf[j];
         lbuf[k+1] = sbuf[j+1];
         if (sbuf[j+m] < 0)
            lbuf[k+2] = minus;
         else
            lbuf[k+2] = plus;
         lbuf[k+3] = lbuf[k+2];
         j -= 2;
         k -= 4;
         }
      }
                                        /* Swap both: S->L            */
   else if (*code == 3) {
      for (i = 0; i < *nvals; i++) {
         temp[1] = sbuf[j];
         temp[0] = sbuf[j+1];
         lbuf[k+1] = temp[1];
         lbuf[k] = temp[0];
         if (temp[1] < 0)
            lbuf[k+2] = minus;
         else
            lbuf[k+2] = plus;
         lbuf[k+3] = lbuf[k+2];
         j -= 2;
         k -= 4;
         }
      }
                                        /* no flip: L->S              */
   else if (*code == 4) {
      for (i = 0; i < *nvals; i++) {
         sbuf[j] = lbuf[k+2];
         sbuf[j+1] = lbuf[k+3];
         j += 2;
         k += 4;
         }
      }
                                        /* byte flip: L->S            */
   else if (*code == 5) {
      for (i = 0; i < *nvals; i++) {
         sbuf[j] = lbuf[k+3];
         sbuf[j+1] = lbuf[k+2];
         j += 2;
         k += 4;
         }
      }
                                        /* word flip: L->S            */
   else if (*code == 6) {
      for (i = 0; i < *nvals; i++) {
         sbuf[j] = lbuf[k];
         sbuf[j+1] = lbuf[k+1];
         j += 2;
         k += 4;
         }
      }
                                        /* both flip: L->S            */
   else if (*code == 7) {
      for (i = 0; i < *nvals; i++) {
         temp[0] = lbuf[k+1];
         temp[1] = lbuf[k];
         sbuf[j] = temp[0];
         sbuf[j+1] = temp[1];
         j += 2;
         k += 4;
         }
      }

   return;
}
