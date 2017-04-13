#define Z_bfli2__
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zbfli2_(int *nb, int *nvals, int *bytflp, char inbuf[],
      char outbuf[])
#else
   void zbfli2_(nb, nvals, bytflp, inbuf, outbuf)
   char inbuf[], outbuf[];
   int *nb, *nvals, *bytflp;
#endif
/*--------------------------------------------------------------------*/
/*! swap 8-bit bytes & 16/32-bit words and move from inbuf to outbuf  */
/*# Z2 Binary                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1998                                               */
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
/*  the same as the input buffer).  The routine is only called by     */
/*  ZBFLIP and only if byte swapping is required on the host machine  */
/*  (e.g., a VAX, PC).  Also does word swaps                          */
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
   int flipcode;
   register int j, k, i, m, n;
   char temp[8], msgbuf[80];
/*--------------------------------------------------------------------*/
   flipcode = *bytflp;
   m = *nb ;
   if ((m != 2) && (m != 4) && (m != 8)) {
      sprintf (msgbuf,
         "ZBYTF2: INVALID VALUE FOR BYTES/VALUE = %d\n", m);
      zmsgwr_ (msgbuf, &n8);
      exit (8);
      }
   n = m / 2 ;
                                        /* no flip, copy if needed    */
   if (flipcode == 0) {
      if (&inbuf[0] == &outbuf[0]) {
	    ;
	 }
      else {
         for (j = 0; j < *nvals; j++) {
            k = m * j;
            for (i = 0 ; i < m ; i++)
               outbuf[k+i] = inbuf[k+i];
            }
         }
      }
                                        /* Swap bytes only.          */
   else if (flipcode == 1) {
      if (&inbuf[0] == &outbuf[0]) {
         for (j = 0; j < *nvals; j++) {
            k = m * j;
            for (i = 0 ; i < n ; i++) {
               temp[0] = inbuf[k];
               temp[1] = inbuf[k+1];
               outbuf[k+1] = temp[0];
               outbuf[k] = temp[1];
               k = k + 2;
               }
            }
         }
      else {
         for (j = 0; j < *nvals; j++) {
            k = m * j;
            for (i = 0 ; i < n ; i++) {
               outbuf[k+1] = inbuf[k];
               outbuf[k] = inbuf[k+1];
               k = k + 2;
               }
            }
         }
      }
                                        /* Swap bytes & words        */
   else if (flipcode == 3) {
      if (&inbuf[0] == &outbuf[0]) {
         for (j = 0; j < *nvals; j++) {
            k = m * j;
            for (i = 0 ; i < m ; i++)
               temp[i] = inbuf[k+i];
            for (i = 0 ; i < m ; i++)
               outbuf[k+i] = temp[m-1-i];
            }
         }
      else {
         for (j = 0; j < *nvals; j++) {
            k = m * j;
            n = k + m - 1;
            for (i = 0 ; i < m ; i++)
               outbuf[k+i] = inbuf[n-i];
            }
         }
      }
                                        /* Swap words only.          */
   else if (flipcode == 2) {
      if (&inbuf[0] == &outbuf[0]) {
         for (j = 0; j < *nvals; j++) {
            k = m * j;
            for (i = 0 ; i < m ; i++)
               temp[i] = inbuf[k+i];
            for (i = 0 ; i < m ; i=i+2) {
               outbuf[k+1] = temp[m-1-i];
               outbuf[k] = temp[m-2-i];
               k = k + 2;
               }
            }
         }
      else {
         for (j = 0; j < *nvals; j++) {
            k = m * j;
            for (i = 0 ; i < m ; i=i+2) {
               outbuf[k+i+1] = inbuf[k+m-1-i];
               outbuf[k+i] = inbuf[k+m-2-i];
               }
            }
         }
      }
                                        /* Invalid "flipcode".        */
   else {
      sprintf (msgbuf,
         "ZBYTF2: INVALID VALUE FOR BYTFLP = %d\n", flipcode);
      zmsgwr_ (msgbuf, &n8);
      exit (8);
      }

   return;
}
