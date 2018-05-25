#define Z_mi2__
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/stat.h>
#include <sys/types.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zmi2_(char *oper, int *fcb, char *buff, int *nblock,
      int *nbytes, int *ierr)
#else
   void zmi2_(oper, fcb, buff, nblock, nbytes, ierr)
   char oper[], buff[];
   int *fcb, *nblock, *nbytes, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! read/write large blocks of data from/to disk, quick return        */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997-1999, 2011-2012                         */
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
/*  Read/write "nbytes" of data from/to disk starting at virtual      */
/*  block "nblock" using "map" I/O (i.e., double buffered,            */
/*  asynchronous I/O).  Block sizes are determined by the value       */
/*  assigned to NBPS as AIPS-bytes in ZDCHIN which is converted to    */
/*  bytes in ZDCHI2 and assigned to Z_nbps.                           */
/*  Inputs:                                                           */
/*     oper     C*4    Operation code "READ" or "WRIT"                */
/*     fcb      I(*)   File control block for open map disk file      */
/*                     and buffer involved                            */
/*     nblock   I      Virtual starting block (1-relative)            */
/*     nbytes   I      Number of AIPS-bytes (1/2 of a local integer)  */
/*                     to be transferred                              */
/*  In/out:                                                           */
/*     buff     I(*)   I/O buffer                                     */
/*  Output:                                                           */
/*     ierr     I      Error return code: 0 => no error               */
/*                        2 => bad opcode                             */
/*                        3 => I/O error (also see ZWAI2)             */
/*                        4 => end of file (see ZWAI2)                */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # of bytes per AIPS-byte   */
   extern int Z_nbpab;
                                        /* # of bytes per disk sector */
   extern int Z_nbps;
   int fd, jerr, n7 = 7, n8 = 8;
   off_t firstbyte, lastbyte, sizebytes, lastiob;
   register int i;
   char op[5], msgbuf[80];
   struct stat fstatbuf;
   float secs = 1.0;
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Form null terminated       */
                                        /* opcode.                    */
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZDAOPN.    */
   fd = *(fcb + Z_fcbfd);
                                        /* Record byte request in the */
                                        /* file control block.        */
   *(fcb + Z_fcbreq) = *nbytes * Z_nbpab;
                                        /* Also, zero error status &  */
                                        /* transfer count entries.    */
   *(fcb + Z_fcberr) = 0;
   *(fcb + Z_fcbxfr) = 0;
                                        /* Zero byte request just     */
                                        /* initiates double buffered  */
                                        /* I/O.                       */
   if (*(fcb + Z_fcbreq) != 0) {
                                        /* Calculate the byte offsets */
                                        /* (0-relative) in the file   */
                                        /* that correspond to the     */
                                        /* I/O request.               */
      firstbyte = (*nblock - 1);
      firstbyte = firstbyte * Z_nbps;
      lastbyte = *(fcb + Z_fcbreq) + firstbyte - 1;
                                        /* Unpack the file size in    */
                                        /* bytes from the file        */
                                        /* control block (as packed   */
                                        /* there in ZDAOPN) and make  */
                                        /* sure that "lastbyte" is    */
                                        /* not beyond the end of the  */
                                        /* file.                      */
      memcpy (&sizebytes, (fcb + Z_fcbsiz), sizeof(off_t)) ;
      memcpy (&lastiob, (fcb + Z_fcbsiz + 2), sizeof(off_t)) ;
      if ((lastbyte + 1) > sizebytes) {
                                        /* Lustre lies try again      */
         sprintf (msgbuf, "ZMI2: DELAY 1");
         zmsgwr_ (msgbuf, &n8);
         zdela2_ (&secs, &jerr);
         if (fstat (fd, &fstatbuf) == -1) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 6;
            return;
            }
         memcpy ((fcb+Z_fcbsiz), &fstatbuf.st_size, sizeof(off_t)) ;
         memcpy (&sizebytes, (fcb + Z_fcbsiz), sizeof(off_t)) ;
         }
      if ((lastbyte + 1) > sizebytes) {
                                        /* Lustre lies try again      */
         sprintf (msgbuf, "ZMI2: DELAY 2");
         zmsgwr_ (msgbuf, &n8);
         zdela2_ (&secs, &jerr);
         if (fstat (fd, &fstatbuf) == -1) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 6;
            return;
            }
         memcpy ((fcb+Z_fcbsiz), &fstatbuf.st_size, sizeof(off_t)) ;
         memcpy (&sizebytes, (fcb + Z_fcbsiz), sizeof(off_t)) ;
         }
                                        /* This is NOT illegal under  */
                                        /* UNIX but IS under AIPS     */
                                        /* design standards, so fake  */
                                        /* it.                        */
      if ((lastbyte + 1) > sizebytes) {
         *(fcb + Z_fcberr) = EINVAL;
         *(fcb + Z_fcbxfr) = -1;
         *ierr = 3;
         if (sizeof(long) < sizeof(off_t)) {
            sprintf (msgbuf,"ZMI2: REQUEST FOR BYTES %lld THRU = %lld",
               firstbyte + 1, lastbyte + 1);
            zmsgwr_ (msgbuf, &n7);
            sprintf (msgbuf,"ZMI2: BEYOND EOF = %lld", sizebytes);
            }
         else if (sizeof(int) < sizeof(long)) {
            sprintf (msgbuf,"ZMI2: REQUEST FOR BYTES %ld THRU = %ld",
               firstbyte + 1, lastbyte + 1);
            zmsgwr_ (msgbuf, &n7);
            sprintf (msgbuf,"ZMI2: BEYOND EOF = %ld", sizebytes);
            }
         else {
            sprintf (msgbuf,"ZMI2: REQUEST FOR BYTES %d THRU = %d",
               firstbyte + 1, lastbyte + 1);
            zmsgwr_ (msgbuf, &n7);
            sprintf (msgbuf,"ZMI2: BEYOND EOF = %d", sizebytes);
            }
         zmsgwr_ (msgbuf, &n7);
         }
                                        /* Set file pointer of the    */
                                        /* file descriptor to the     */
                                        /* absolute byte address      */
                                        /* "firstbyte".               */
      else {
         if (firstbyte != lastiob+1) {
            if (lseek (fd, firstbyte, 0) == -1) {
               *(fcb + Z_fcberr) = errno;
               if (sizeof(long) < sizeof(off_t))
                  sprintf (msgbuf,
                  "ZMI2: LSEEK ERROR FILE DESCRIPTOR %d TO POSITION %lld",
                     fd, firstbyte);
               else if (sizeof(int) < sizeof(long))
                  sprintf (msgbuf,
                  "ZMI2: LSEEK ERROR FILE DESCRIPTOR %d TO POSITION %ld",
                     fd, firstbyte);
               else
                  sprintf (msgbuf,
                  "ZMI2: LSEEK ERROR FILE DESCRIPTOR %d TO POSITION %d",
                     fd, firstbyte);
               zmsgwr_ (msgbuf, &n7);
               *ierr = 3;
               }
            }
                                        /* Perform the I/O and record */
                                        /* the # of bytes transferred */
                                        /* in *(fcb + Z_fcbxfr).      */
                                        /* Record the system error    */
                                        /* code (if any) in           */
                                        /* *(fcb + Z_fcberr).         */
         if (*ierr == 0) {
            if (strcmp (op, "READ") == 0) {
               if ((*(fcb + Z_fcbxfr) =
                  read (fd, buff, *(fcb + Z_fcbreq))) == -1) {
                  *(fcb + Z_fcberr) = errno;
                  *ierr = 3;
                  }
               }
            else if (strcmp (op, "WRIT") == 0) {
               if ((*(fcb + Z_fcbxfr) =
                  write (fd, buff, *(fcb + Z_fcbreq))) == -1) {
                  *(fcb + Z_fcberr) = errno;
                  *ierr = 3;
                  }
               }
                                        /* Bad opcode.                */
            else {
               *ierr = 2;
               }
            }
         }

      if (*ierr == 0)
         memcpy ((fcb + Z_fcbsiz + 2), &lastbyte, sizeof(off_t)) ;
      }

   return;
}
