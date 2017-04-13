#include <errno.h>
#define MAXPNAME 48                     /* Maximum logical device     */
                                        /* plus file name length.     */
#define MAXPATH 128                     /* Maximum path name length.  */

zcmpr2_(fcb, pname, nblocks, map, syserr, ierr)
/*--------------------------------------------------------------------*/
/*! truncate a disk file, returning blocks to the system              */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997                                               */
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
/* Truncates a disk file.  It is assumed that the file is open.       */
/* Inputs:                                                            */
/*    fcb     I(*)   File descriptor from FTAB file control block     */
/*    pname   C*48   File name                                        */
/*    nblocks I      Desired size in 256-integer blocks               */
/*    map     I      I/O type: 0 => non-map (single buffered)         */
/*                             1 => map (double buffered)             */
/* Output:                                                            */
/*    ierr    I      Error code: 0 = success                          */
/*                      1 = file name translation fails               */
/*                      2 = error occurred                            */
/*                      3 = temporary file not deleted                */
/* UNICOS version (Berkeley UNIX "ftruncate" supported).              */
/* really the same as the unix version                                */
/*--------------------------------------------------------------------*/
int *fcb, *nblocks, *syserr, *ierr;
char pname[MAXPNAME];
/*--------------------------------------------------------------------*/
{
                                        /* number bytes per AIPS byte */
   extern int  Z_fcbfd, Z_fcbsiz, Z_fcberr, Z_mfcb, Z_nbpab ;
   off_t nbytes, lastiob;
   int fd;
   struct stat fstatbuf;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   *syserr = 0;
   nbytes = *nblocks;
   nbytes = nbytes * 512 * Z_nbpab;
   fd = *(fcb + Z_fcbfd);
                                        /* Truncate file to "nbytes". */
   if (ftruncate (*fcb, nbytes) == -1) {
      *ierr = 2;
      *syserr = errno;
      }
                                        /* Squirrel away file size    */
   else if (fstat (fd, &fstatbuf) == -1) {
      *(fcb + Z_fcberr) = errno;
      *ierr = 6;
      }
   else {
      lastiob = -100;
      *nblocks = fstatbuf.st_size / 512 / Z_nbpab ;
      memcpy ((fcb+Z_fcbsiz), &fstatbuf.st_size, sizeof(off_t)) ;
      memcpy ((fcb+Z_fcbsiz+2), &lastiob, sizeof(off_t)) ;
      if (*map == 1) {
         memcpy ((fcb+Z_mfcb+Z_fcbsiz), &fstatbuf.st_size, sizeof(off_t)) ;
         memcpy ((fcb+Z_mfcb+Z_fcbsiz+2), &lastiob, sizeof(off_t)) ;
         }
      }

   return;
}
