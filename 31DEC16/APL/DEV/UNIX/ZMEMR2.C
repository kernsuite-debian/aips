#define Z_memr2__
#include <stddef.h>
#include <stdlib.h>

#if __STDC__
   void zmemr2_(int *get, int *kbytes, int *base, long *addr,
      long *offset, int *ierr)
#else
   void zmemr2_(get, kbytes, base, addr, offset, ierr)
   int *get, *kbytes, *base, *ierr;
   long *addr, *offset;
#endif
/*--------------------------------------------------------------------*/
/*! get or free memory                                                */
/*# Z2 System                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1996-1997, 2005, 2009                              */
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
/*  Allocates or frees memory blocks                   .              */
/*  Inputs:                                                           */
/*     get     I      0 get memory, 1 free memory                     */
/*     kbytes  I      real KILO bytes to get, ignored on free         */
/*     base    I      address of Fortran "buffer"                     */
/*  In/out:                                                           */
/*     addr    L      Out on get: address pointer allocated (a long)  */
/*                    In on free: address pointer to free             */
/*  Output:                                                           */
/*     offset  L      get only: subscript in Fortran buffer for start */
/*                       of allocated memory                          */
/*     ierr    I      Error return code: 0 => no error                */
/*                       1 => error                                   */
/*  Generic UNIX version                                              */
/*--------------------------------------------------------------------*/
{
   int *baddr;
   long  bytes;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   bytes = *kbytes;
   bytes = bytes * 1024;
                                        /* allocate memory            */
   if (*get == 0) {
      if ((baddr = (int *) malloc (bytes)) == NULL)
         *ierr = 1;
                                        /* return the address, offset */
      else {
         *offset = (baddr - base) ;
         *addr = (long) baddr;
         }
      }
                                        /* Free up the buffer.        */
   else {
      baddr = (int *) *addr;
      free (baddr);
      }
                                        /* Exit.                      */
   return;
}
