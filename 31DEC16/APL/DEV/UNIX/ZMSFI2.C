#define Z_msfi2__
#include <unistd.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <sys/types.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zmsfi2_(char *oper, int *fcb, char *buff, int *nrec,
      int *ierr)
#else
   void zmsfi2_(oper, fcb, buff, nrec, ierr)
   char oper[], buff[];
   int *fcb, *nrec, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! read/write one 256-integer record from/to a non-map disk file     */
/*# IO-basic                                                          */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995-1997, 1999, 2011                              */
/*;  Associated Universities, Inc. Washington DC, USA                 */
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
/*  Transfer one 256 local integer record between an I/O buffer and   */
/*  disk starting at random access record "nrec" using "non-map" I/O  */
/*  (i.e., single buffered, synchronous I/O).                         */
/*  Inputs:                                                           */
/*     oper   C*4    Operation code "READ" or "WRIT"                  */
/*     fcb    I(*)   File control block for opened non-map disk file  */
/*     nrec   I      Disk record number (1-relative, units of         */
/*                   256-integer records)                             */
/*  In/out:                                                           */
/*     buff   I(256) I/O buffer                                       */
/*  Output:                                                           */
/*     ierr   I      Error return code: 0 => no error                 */
/*                      2 => bad opcode                               */
/*                      3 => I/O error                                */
/*                      4 => end of file                              */
/*                   1000 + n => read only n (< 256) words)           */
/*  Generic UNIX version: no MSGWRT calls!                            */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
                                        /* # of bytes per word        */
   extern int Z_nbpwd;
   off_t firstbyte, lastbyte, sizebytes, checkbytes, lastiob;
   int fd;
   register int i;
   char op[5];
/*--------------------------------------------------------------------*/
                                        /* Form null terminated       */
                                        /* opcode.                    */
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Assume success.            */
   *ierr = 0;
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZDAOPN.    */
   fd = *(fcb + Z_fcbfd);
                                        /* Record byte request in the */
                                        /* file control block (always */
                                        /* one 256 local integer      */
                                        /* record).                   */
   *(fcb + Z_fcbreq) = 256 * Z_nbpwd;
                                        /* Also, zero error status &  */
                                        /* transfer count entries.    */
   *(fcb + Z_fcberr) = 0;
   *(fcb + Z_fcbxfr) = 0;
                                        /* Calculate the byte offsets */
                                        /* (0-relative) in the file   */
                                        /* that correspond to the     */
                                        /* requested record.          */
   firstbyte = *nrec - 1;
   firstbyte = (firstbyte * 256) * Z_nbpwd;
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
   checkbytes = lastbyte;
   if (strcmp (op, "READ") == 0) checkbytes = firstbyte;
                                        /* test beyond EOF remember   */
                                        /* size is 1-rel              */
   if (checkbytes >= sizebytes) {
                                        /* This is NOT illegal under  */
                                        /* UNIX but IS under AIPS     */
                                        /* standards, so fake it.     */
      *(fcb + Z_fcberr) = EINVAL;
      *(fcb + Z_fcbxfr) = -1;
      *ierr = 3;
      if (sizeof(long) < sizeof(off_t)) {
         fprintf (stderr,"ZMSFI2: REQUEST FOR BYTES %lld THRU = %lld\n",
            firstbyte + 1, lastbyte + 1);
         fprintf (stderr,"ZMSFI2: BEYOND EOF = %lld\n", sizebytes);
         }
      else if (sizeof(int) < sizeof(long)) {
         fprintf (stderr,"ZMSFI2: REQUEST FOR BYTES %ld THRU = %ld\n",
            firstbyte + 1, lastbyte + 1);
         fprintf (stderr,"ZMSFI2: BEYOND EOF = %ld\n", sizebytes);
         }
      else {
         fprintf (stderr,"ZMSFI2: REQUEST FOR BYTES %d THRU = %d\n",
            firstbyte + 1, lastbyte + 1);
         fprintf (stderr,"ZMSFI2: BEYOND EOF = %d\n", sizebytes);
         }
      }
                                        /* Set file pointer of the    */
                                        /* file descriptor to the     */
                                        /* absolute byte address      */
                                        /* "firstbyte".               */
   else {
      if (firstbyte != lastiob+1) {
         if (lseek (fd, firstbyte, 0) == -1) {
            *(fcb + Z_fcberr) = errno;
            *ierr = 3;
            if (sizeof(long) < sizeof(off_t))
               fprintf (stderr,
               "ZMSFI2: LSEEK ERROR FILE DESCRIPTOR %d TO POSITION %lld\n",
                  fd, firstbyte);
            else if (sizeof(int) < sizeof(long))
               fprintf (stderr,
               "ZMSFI2: LSEEK ERROR FILE DESCRIPTOR %d TO POSITION %ld\n",
                  fd, firstbyte);
            else
               fprintf (stderr,
               "ZMSFI2: LSEEK ERROR FILE DESCRIPTOR %d TO POSITION %d\n",
                  fd, firstbyte);
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
            if (lastbyte >= sizebytes)
               *(fcb + Z_fcbreq) = sizebytes - firstbyte;
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
            return;
            }
         }
      }
                                        /* End of file?               */
   if (*(fcb + Z_fcbxfr) == 0)
      *ierr = 4;
                                        /* Partial read?              */
   if ((*ierr == 0) && (*(fcb + Z_fcbxfr) != 256 * Z_nbpwd))
      *ierr = *(fcb + Z_fcbxfr) / Z_nbpwd + 1000;
   if (*ierr == 0)
      memcpy ((fcb + Z_fcbsiz + 2), &lastbyte, sizeof(off_t)) ;

                                        /* Exit.                      */
   return;
}
