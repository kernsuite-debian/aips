#define Z_exis2__
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zexis2_(char *pname, int *isize, int *syserr, int *ierr)
#else
   void zexis2_(pname, isize, syserr, ierr)
   char pname[];
   int *isize, *syserr, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! return size of disk file and if  it exists                        */
/*# IO=basic                                                          */
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
/*  Determine the existence of a file and its size in AIPS-bytes      */
/*  (1/2 of a local integer).                                         */
/*  Inputs:                                                           */
/*     pname   C*48   Physical file name                              */
/*  Output:                                                           */
/*     size    I      File size 256-int blocks (if exists, else 0)    */
/*     syserr  I      System error code                               */
/*     ierr    I      Error return code: 0 => file exists             */
/*                       1 => file does not exist                     */
/*                       2 => inputs error                            */
/*                       3 => other error                             */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
                                        /* # bytes per AIPS-byte      */
   extern int Z_nbpab;
   int llen, xlen, xlnb, jerr ;
   char pathname[MAXAPATH] ;
   struct stat statbuf;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *syserr = 0;
                                        /* Translate logical device   */
                                        /* name and null terminate.   */
   llen = MAXPNAME;
   xlen = MAXAPATH;
   zpath_ (&llen, pname, &xlen, pathname, &xlnb, &jerr);
   if (jerr != 0) {
      *ierr = 2;
      }
   else {
      pathname[xlnb] = '\0';
                                        /* "Stat" full path name.     */
      if (stat (pathname, &statbuf) == -1) {
         *isize = 0;
         *syserr = errno ;
                                        /* File doesn't exist.        */
         if (errno == ENOENT)
            *ierr = 1;
                                        /* Some other "stat" error.   */
         else
            *ierr = 3;
         }
      else
         *isize = statbuf.st_size / Z_nbpab / 512;
      }

   return;
}
