#define Z_expn2__
#include <stdio.h>
#include <errno.h>
#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <stdlib.h>
#include <unistd.h>

#if __STDC__
   void zexpnm_(int *fcb, int *map, int *nrecs, int *ierr)
#else
   void zexpnm_(fcb, map, nrecs, ierr)
   int *fcb, *map, *nrecs, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! expand an open disk file                                          */
/*# IO=basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2005, 2011                             */
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
/*  Expands an open disk file, reserving the disk space.              */
/*                                                                    */
/*  Inputs:                                                           */
/*     fcb     I(*)   File control block for opened disk file         */
/*     map     I      Double buffer file? 1 => yes, 0 => no           */
/*  In/out:                                                           */
/*     nrecs   I      In:  number of 256-integer records to expand    */
/*                    Out: number of records expanded                 */
/*  Output:                                                           */
/*     ierr    I      Error return code: 0 => no error                */
/*                       1 => error                                   */
/*  Generic UNIX version - The file is written to do the expansion.   */
/*  for MSGWRT                                                        */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # of FTAB elements per map */
                                        /* file control block         */
   extern int Z_mfcb;
                                        /* # of bytes per word        */
   extern int Z_nbpwd;
   int fd, recsiz, nby, jerr;
   off_t size, lastiob;
   register int i;
   char *buff = NULL;
   struct stat fstatbuf;
   float secs = 1.0;
/*--------------------------------------------------------------------*/
   *ierr = 0;
   lastiob = -100;
                                        /* Position to the end of the */
                                        /* file.                      */
   fd = *(fcb + Z_fcbfd);
   if (lseek (fd, 0L, SEEK_END) == -1) {
      *(fcb + Z_fcberr) = errno;
      *ierr = 1;
      }
                                        /* Allocate buffer of size    */
                                        /* "recsiz" bytes.            */
   else {
      recsiz = 256 * Z_nbpwd;
      if ((buff = (char *) malloc (recsiz)) == NULL) {
         *(fcb + Z_fcberr) = errno;
         *ierr = 1;
         }
                                        /* Expand the file by "nrecs".*/
      else {
         memset (buff, 0, recsiz);
         for (i = 0; i < *nrecs; i++) {
            if ((nby = write (fd, buff, recsiz)) == -1) {
               *(fcb + Z_fcberr) = errno;
               *(fcb + Z_fcbxfr) = nby;
               *ierr = 1;
               break;
               }
            }
                                        /* Free up the buffer.        */
         free (buff);
         }
      }
                                        /* Get new file size.         */
   if ((nby = fstat (fd, &fstatbuf)) == -1) {
      *(fcb + Z_fcberr) = errno;
      *(fcb + Z_fcbxfr) = nby;
      *ierr = 1;
      }
   else {
                                        /* Unpack old size pack new   */
                                        /* in file control block.     */
      memcpy (&size, (fcb + Z_fcbsiz), sizeof(off_t)) ;
      if ((fstatbuf.st_size-size) <= 0) {
         zdela2_ (&secs, &jerr);
         if ((nby = fstat (fd, &fstatbuf)) == -1) {
            *(fcb + Z_fcberr) = errno;
            *(fcb + Z_fcbxfr) = nby;
            *ierr = 1;
            return;
            }
         }
      if ((fstatbuf.st_size-size) <= 0) {
         zdela2_ (&secs, &jerr);
         if ((nby = fstat (fd, &fstatbuf)) == -1) {
            *(fcb + Z_fcberr) = errno;
            *(fcb + Z_fcbxfr) = nby;
            *ierr = 1;
            return;
            }
         }
      memcpy ((fcb + Z_fcbsiz), &fstatbuf.st_size, sizeof(off_t)) ;
      memcpy ((fcb + Z_fcbsiz + 2), &lastiob, sizeof(off_t)) ;
                                        /* Calculate expansion in     */
                                        /* units of "recsiz" records. */
      *nrecs = (fstatbuf.st_size - size) / recsiz;
      if (*map == 1) {
         memcpy ((fcb+Z_mfcb+Z_fcbsiz), &fstatbuf.st_size, sizeof(off_t));
         memcpy ((fcb+Z_mfcb+Z_fcbsiz + 2), &lastiob, sizeof(off_t)) ;
         }
      }
                                        /* Exit.                      */
   return;
}
