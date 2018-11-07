#define Z_xdrst__
#include <string.h>
#include <rpc/rpc.h>
#include <rpc/xdr.h>

#define MAXL   8192

#if __STDC__
   void zxdrst_ (char *op, char *lstr, int *len, int *ibuf, int *nbuf,
      int *nbytes, int *ierr)
#else
   void zxdrst_ (op, lstr, len, ibuf, nbuf, nbytes, ierr)
   char *op, *lstr;
   int *len, *ibuf, *nbuf, *nbytes, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! Encode/decode a character string in XDR format                    */
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
/*  Encode/decode a character string in XDR format                    */
/*  Inputs:                                                           */
/*     op       C*6        Opcode ("ENCODE" or "DECODE")              */
/*     nbuf     I          Dimension of IBUF in words (1-relative)    */
/*  Input/output:                                                     */
/*     lstr     C*(len)    Input character string                     */
/*     len      I          Length of "lstr" (1-relative)              */
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
   char slocal[MAXL+1], opcode[7];
   int size, i, encode, decode, clen;
   unsigned int xdrpos;
   char *nptr = slocal;
   char **mptr = &nptr;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                         /* Convert FORTRAN opcode    */
   for (i = 0; i < 6; i++)
      opcode[i] = op[i];
   opcode[6] = '\0';
                                         /* Encode or decode ?        */
   encode = (strcmp (opcode, "ENCODE") == 0);
   decode = (strcmp (opcode, "DECODE") == 0);
                                         /* Form NULL-terminated copy */
                                         /* of input FORTRAN string   */
                                         /* if encoding               */
   if (encode) {
      for (i = 0; i < *len; i++)
         slocal[i] = lstr[i];
      lstr[*len] = '\0';
    };
                                         /* Size of XDR buff. (bytes) */
   size = *nbuf * Z_nbitwd / 8;
                                         /* Create XDR encode/decode  */
                                         /* stream  in memory         */
   if (encode)
      xdrmem_create (&xdrs, (char *) ibuf, size, XDR_ENCODE);
   else
      xdrmem_create (&xdrs, (char *) ibuf, size, XDR_DECODE);
                                         /* Re-position XDR stream    */
   xdrpos = (unsigned int) *nbytes;
   if (xdr_setpos (&xdrs, xdrpos) == FALSE)
      *ierr = 1;
   else {
                                         /* Append or read NULL-      */
                                         /* terminated string         */
      if (xdr_wrapstring (&xdrs, mptr) == FALSE)
         *ierr = 2;
      else {
                                         /* Return new XDR position   */
         xdrpos = xdr_getpos (&xdrs);
         *nbytes = (unsigned int) xdrpos;
                                         /* Close XDR stream          */
         xdr_destroy (&xdrs);
       };
    };
                                         /* Truncate NULL from output */
                                         /* string returned if decode */
   if (*ierr == 0 && decode) {
      clen = *len;
      *len = strlen (slocal);
      for (i = 0; i < *len; i++)
         lstr[i] = slocal[i];
                                         /* Pad output FORTRAN string */
      for (i = *len; i < clen; i++)
         lstr[i] = ' ';
    };
   return;
 }


