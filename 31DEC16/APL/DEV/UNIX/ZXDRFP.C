#define Z_xdrfp__
#include <string.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>

#if __STDC__
   void zxdrfp_ (char *op, float *val, int *len, int *ibuf, int *nbuf,
      int *nbytes, int *ierr)
#else
   void zxdrfp_ (op, val, len, ibuf, nbuf, nbytes, ierr)
   char *op;
   float *val;
   int *len, *ibuf, *nbuf, *nbytes, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! Encode/decode a float array in XDR format                         */
/*# Z IO-remote                                                       */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1997                                               */
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
/*  Encode/decode a float array in XDR format                         */
/*  Inputs:                                                           */
/*     op       C*6        Opcode ('ENCODE' or 'DECODE')              */
/*     len      I          Dimension of val (1-relative)              */
/*     nbuf     I          Dimension of ibuf in words (1-relative)    */
/*  Input/Output:                                                     */
/*     val      R(*)       Input float array                          */
/*     ibuf     I(*)       XDR buffer                                 */
/*     nbytes   I          Current ptr in XDR buffer (1-rel. bytes)   */
/*  Output:                                                           */
/*     ierr     I          Return code (0=> ok; else error)           */
/*  Generic UNIX version                                              */
/*--------------------------------------------------------------------*/
{
                                         /* No. of bits per word      */
   extern int Z_nbitwd;
   XDR xdrs;
   char opcode[6];
   int size, i, k;
   unsigned int xdrpos;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                         /* Convert input FORTRAN str.*/
                                         /* to NULL-term. C string    */
   for (i = 0; i < 6; i++)
      opcode[i] = op[i];
                                         /* Size of XDR buff. (bytes) */
   size = *nbuf * Z_nbitwd / 8;
                                         /* Create XDR encode/decode  */
                                         /* stream in memory          */
   if (strcmp (opcode, "ENCODE") == 0)
      xdrmem_create (&xdrs, (char *) ibuf, size, XDR_ENCODE);
   else
      xdrmem_create (&xdrs, (char *) ibuf, size, XDR_DECODE);
                                         /* Re-position XDR stream    */
   xdrpos = (unsigned int) *nbytes;
   if (xdr_setpos (&xdrs, xdrpos) == FALSE)
      *ierr = 1;
   else {
                                         /* Append/read float array   */
      i = 0;
      k = xdr_float (&xdrs, &(val[i]));
      while ((i < (*len-1)) && (k == TRUE)) {
         i++;
         k = xdr_float (&xdrs, &(val[i]));
       };
                                         /* Check for XDR error       */
      if (k == FALSE)
         *ierr = 2;
      else {
                                         /* Return new XDR position   */
         xdrpos = xdr_getpos (&xdrs);
         *nbytes = (unsigned int) xdrpos;
                                         /* Close XDR stream          */
         xdr_destroy (&xdrs);
       };
    };
   return;
 }



