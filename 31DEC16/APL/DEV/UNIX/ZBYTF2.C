#define Z_bytf2__
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zbytf2_(int *nwords, int *bytflp, int *nbitwd, char inbuf[],
      char outbuf[])
#else
   void zbytf2_(nwords, bytflp, nbitwd, inbuf, outbuf)
   char inbuf[], outbuf[];
   int *nwords, *bytflp, *nbitwd;
#endif
/*--------------------------------------------------------------------*/
/*! swap low and high order 8-bit bytes and move from inbuf to outbuf */
/*# Z2 Binary                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997                                          */
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
/*  ZBYTFL and only if byte swapping is required on the host machine  */
/*  (e.g., a VAX).                                                    */
/*  Inputs:                                                           */
/*     nwords   I       Length of input buffer in 16-bit "words"      */
/*     bytflp   I       Byte/words swapped indicator as set in ZDCHIN */
/*                        1 => byte swapped only                      */
/*                        2 => word swapped only                      */
/*                        3 => both byte and word swapped             */
/*     nbitwd   I       Number of bits per integer                    */
/*     inbuf    I*2(*)  input buffer                                  */
/*  Output:                                                           */
/*     outbuf   I*2(*)  output buffer containing swapped words        */
/*                        (may be the same as the input buffer)       */
/* Generic UNIX version (only performs byte-swapping at present;  it  */
/* may be possible to use the UNIX library function "swab" for this   */
/* it's not clear from the documentation if inbuf and outbuf can be   */
/* the same).                                                         */
/* This will probably do bad things if the arrays overlap but don't   */
/* start at the same place.                                           */
/*--------------------------------------------------------------------*/
{
   int n8 = 8;
   int flipcode;
   register int j, k;
   char temp[2], msgbuf[80];
/*--------------------------------------------------------------------*/
   flipcode = *bytflp;
                                        /* Swap bytes only.  If      */
                                        /* inbuf is the same as      */
                                        /* outbuf, use temporary     */
                                        /* storage.                  */
   if ((flipcode == 1) || (flipcode == 3)) {
      if (&inbuf[0] == &outbuf[0]) {
         for (j = 0; j < *nwords; j++) {
            k = 2 * j;
            temp[0] = inbuf[k];
            temp[1] = inbuf[k+1];
            outbuf[k] = temp[1];
            outbuf[k+1] = temp[0];
            }
         }
      else {
         for (j = 0; j < *nwords; j++) {
            k = 2 * j;
            outbuf[k+1] = inbuf[k];
            outbuf[k] = inbuf[k+1];
            }
         }
      }
   else if ((flipcode == 2) || (flipcode == 0)) {
      if (&inbuf[0] == &outbuf[0]) {
	    ;
	 }
      else {
         for (j = 0; j < *nwords; j++) {
            k = 2 * j;
            outbuf[k+1] = inbuf[k+1];
            outbuf[k] = inbuf[k];
            }
         }
      }
   else {
                                        /* Invalid "flipcode".        */
      sprintf (msgbuf,
         "ZBYTF2: INVALID VALUE FOR BYTFLP = %d\n", flipcode);
      zmsgwr_ (msgbuf, &n8);
      exit (8);
      }

   return;
}
