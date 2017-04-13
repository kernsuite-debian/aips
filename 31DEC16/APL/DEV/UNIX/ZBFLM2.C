#define Z_bflm2__
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zbflm2_(int *nb, int *nvals, int *bytflp, char inbuf[],
      char outbuf[])
#else
   void zbflm2_(nb, nvals, bytflp, inbuf, outbuf)
   char inbuf[], outbuf[];
   int *nb, *nvals, *bytflp;
#endif
/*--------------------------------------------------------------------*/
/*! swap 8-bit bytes and 32/64-bit words if not INDE                  */
/*# Z2 Binary                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 2007                                               */
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
/*  Interchange the low order and high order bytes for all words in   */
/*  the input buffer and puts results in output buffer (which may be  */
/*  the same as the input buffer).  If the word is "INDEFINT", it is  */
/*  not flipped.  Debugged only for bytflp = 3.  Also does word swaps */
/*  Inputs:                                                           */
/*     nb       I      Number of bytes in word (2,4,8 supported)      */
/*     nvals    I      Length of input buffer in nb-bit "words"       */
/*     bytflp   I      Byte/words swapped indicator as set in ZDCHIN  */
/*                        0 => neither are swapped                    */
/*                        1 => byte swapped only                      */
/*                        2 => word swapped only                      */
/*                        3 => both byte and word swapped             */
/*                        All are supported here                      */
/*     inbuf    I(*)   input buffer                                   */
/*  Output:                                                           */
/*     outbuf   I(*)   output buffer containing swapped words         */
/*                        (may be the same as the input buffer)       */
/* Generic UNIX version                                               */
/*--------------------------------------------------------------------*/
{
   int n8 = 8;
   int flipcode, allchar;
   register int j, k, i, m, n;
   char temp[8], msgbuf[80], INDEF[]="INDEFINT";
/*--------------------------------------------------------------------*/
   flipcode = *bytflp;
   m = *nb ;
   if ((m != 2) && (m != 4) && (m != 8)) {
      sprintf (msgbuf,
         "ZBFLC2: INVALID VALUE FOR BYTES/VALUE = %d\n", m);
      zmsgwr_ (msgbuf, &n8);
      exit (8);
      }
   n = m / 2 ;
                                        /* no flip, copy if needed    */
   if (flipcode == 0) {
      if (&inbuf[0] != &outbuf[0]) {
         k = m * (*nvals);
         for (j = 0; j < k; j++)
            outbuf[k] = inbuf[k];
         }
      }
                                       /* swap something            */
   else {
      for (j = 0; j < *nvals; j++) {
         k = m * j;
         allchar = 1;
         for (i = 0 ; i < m ; i++) {
            temp[i] = inbuf[k+i];
            if (temp[i] != INDEF[i]) allchar = 0;
            }
                                       /* Copy characters only      */
         if (allchar) {
            for (i = 0 ; i < m ; i++)
               outbuf[k+i] = temp[i];
            }
                                       /* Swap bytes only.          */
         else if (flipcode == 1) {
            for (i = 0 ; i < n ; i=i+2) {
               outbuf[k+1] = temp[i];
               outbuf[k] = temp[i+1];
               k = k + 2;
               }
            }
                                        /* Swap bytes & words        */
         else if (flipcode == 3) {
            for (i = 0 ; i < m ; i++)
               outbuf[k+i] = temp[m-1-i];
            }
                                        /* Swap words only.          */
         else if (flipcode == 2) {
            for (i = 0 ; i < m ; i=i+2) {
               outbuf[k+1] = temp[m-1-i];
               outbuf[k] = temp[m-2-i];
               k = k + 2;
               }
            }
                                        /* Invalid "flipcode".        */
         else {
            sprintf (msgbuf,
               "ZBFLM2: INVALID VALUE FOR BYTFLP = %d\n", flipcode);
            zmsgwr_ (msgbuf, &n8);
            exit (8);
            }
         }
      }

   return;
}
