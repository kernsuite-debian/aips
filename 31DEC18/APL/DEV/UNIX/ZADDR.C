#define Z_addr__
#if __STDC__
   void zaddr_(char *addr1, char *addr2, int *ierr)
#else
   void zaddr_(addr1, addr2, ierr)
   char *addr1, *addr2 ;
   int *ierr ;
#endif
/*--------------------------------------------------------------------*/
/*! determine if 2 addresses inside computer are the same             */
/*# Z Service                                                         */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997-1998                                         */
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
/*  Determine if two addresses are the same.                          */
/*  Inputs:                                                           */
/*     addr1   I(*)   Address 1                                       */
/*     addr2   I(*)   Address 2                                       */
/*  Output:                                                           */
/*     ierr    I      Error return code:                              */
/*                       0 => addresses are the same                  */
/*                       1 => addresses are different                 */
/*                                                                    */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
                                        /* Assume the addresses are   */
                                        /* different.                 */
   *ierr = 1;
                                        /* Test to see if the         */
                                        /* addresses are the same.    */
   if (addr1 == addr2) *ierr = 0;
                                        /* Exit.                      */
   return;
}
