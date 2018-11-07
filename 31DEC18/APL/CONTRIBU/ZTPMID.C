#include <errno.h>

ztpmid_(oper, blkno, fcb, buff, nbytes, ierr)
/*--------------------------------------------------------------------*/
/*! pseudo-tape disk read/write for 2880-bytes records                */
/*# Z2 Tape FITS                                                      */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1996                                               */
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
/*--------------------------------------------------------------------*/
/*  Low level sequential access, large record, double buffered        */
/*  pseudo-tape disk I/O.  Actual implementation is often via Fortran */
/*  IO and hence may not actually done with quick return.             */
/*                                                                    */
/*  NOTE: This is for TAPIO type operations only, i.e., LRECL=2880,   */
/*        FTAB(FIND+5) = I   logical record number.                   */
/*                                                                    */
/*  Inputs:                                                           */
/*     oper     C*4    Operation code "READ" or "WRIT"                */
/*     blkno    I      Virtual 2880 8-bit-byte block number           */
/*     fcb      I(*)   File control block for open map disk file and  */
/*                     buffer involved                                */
/*     nbytes   I      Number of 8-bit bytes to transfer: 0, 2880 ok  */
/*  In/out:                                                           */
/*     buff     I(*)   I/O buffer                                     */
/*  Output:                                                           */
/*     ierr     I      Error return code: 0 => no error               */
/*                        "fcb" has syserr for ZTPWAD to return error */
/*  UNICOS version (synchronous I/O).                                 */
/*--------------------------------------------------------------------*/
char oper[], buff[];
int *blkno, *fcb, *nbytes, *ierr;
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
   extern int errno;
   int offset, fd, n7 = 7, lbytes, mbytes, ntry;
   register int i;
   char op[5], msgbuf[80];
   char *pb;
   int lseek();
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Form null terminated       */
                                        /* opcode.                    */
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZTPOPD.    */
   fd = *(fcb + Z_fcbfd);
                                        /* Record byte request in the */
                                        /* file control block.        */
   *(fcb + Z_fcbreq) = *nbytes;
                                        /* Also, zero error status &  */
                                        /* transfer count entries.    */
   *(fcb + Z_fcberr) = 0;
   *(fcb + Z_fcbxfr) = 0;
                                        /* Zero byte request just     */
                                        /* initiates double buffered  */
                                        /* I/O.                       */
   if (*(fcb + Z_fcbreq) != 0) {
                                        /* Only non-zero byte reqest  */
                                        /* is 2880.                   */
      if (*(fcb + Z_fcbreq) != 2880) {
         *ierr = 2;
         }
      else {
                                        /* Calculate the byte offset  */
                                        /* (0-relative) in the file   */
                                        /* that correspond to the     */
                                        /* virtual block number.      */
         offset = (*blkno - 1) * 2880;
                                        /* Set file pointer of the    */
                                        /* file descriptor to the     */
                                        /* absolute byte address      */
                                        /* "offset".                  */
         if (lseek (fd, offset, 0) == -1) {
            *(fcb + Z_fcberr) = errno;
            sprintf (msgbuf,
               "ZTPMID: LSEEK ERROR ON FILE DESCRIPTOR %d", fd);
            zmsgwr_ (msgbuf, &n7);
            sprintf (msgbuf,
               "ZTPMID: POSITIONING FILE POINTER TO BYTE OFFSET %d",
               offset);
            zmsgwr_ (msgbuf, &n7);
            }
                                        /* Perform the I/O and record */
                                        /* the # of bytes transferred */
                                        /* in *(fcb + Z_fcbxfr).      */
                                        /* Record the system error    */
                                        /* code (if any) in           */
                                        /* *(fcb + Z_fcberr).         */
         else {
            mbytes = *(fcb + Z_fcbreq);
            pb = buff;
            ntry = 10;
            if (strcmp (op, "READ") == 0) {
               if ((mbytes > 0) && (ntry > 0)) {
                  if ((lbytes = read (fd, pb, mbytes)) == -1) {
                     *(fcb + Z_fcbxfr) = *(fcb + Z_fcbreq) - mbytes;
                     *(fcb + Z_fcberr) = errno;
                     *ierr = 3;
                     ntry = -1;
                     }
                  else {
                     mbytes = mbytes - lbytes;
                     *(fcb + Z_fcbxfr) = *(fcb + Z_fcbreq) - mbytes;
                     pb = pb + lbytes;
                     if (lbytes == 0) ntry = -1;
                     }
                  }
               }
            else if (strcmp (op, "WRIT") == 0) {
               if ((mbytes > 0) && (ntry > 0)) {
                  if ((lbytes = write (fd, pb, mbytes)) == -1) {
                     *(fcb + Z_fcbxfr) = *(fcb + Z_fcbreq) - mbytes;
                     *(fcb + Z_fcberr) = errno;
                     *ierr = 3;
                     ntry = -1;
                     }
                  else {
                     mbytes = mbytes - lbytes;
                     *(fcb + Z_fcbxfr) = *(fcb + Z_fcbreq) - mbytes;
                     pb = pb + lbytes;
                     if (lbytes == 0) ntry = ntry - 1;
                     }
                  }
               }
                                        /* Bad opcode.                */
            else {
               *ierr = 2;
               }
            }
         }
      }

   return;
}
