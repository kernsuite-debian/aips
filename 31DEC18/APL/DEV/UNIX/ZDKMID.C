#define Z_dkmid__
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <sys/types.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zdkmid_(char *oper, int *blkno, int *fcb, char *buff,
      int *nbytes, int *ierr)
#else
   void zdkmid_(oper, blkno, fcb, buff, nbytes, ierr)
   char oper[], buff[];
   int *fcb, *blkno, *nbytes, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! read 2048 byte blocks of data from disk                           */
/*# Z Tape                                                            */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 2002                                               */
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
/*  Read nbytes of data from/ disk at FILLM block blkno               */
/*                                                                    */
/*  NOTE: This is for TAPIO type operations only, i.e., LRECL=22048   */
/*        FTAB(FIND+5) = I   logical record number.                   */
/*                                                                    */
/*  Inputs:                                                           */
/*     oper     C*4    Operation code "READ"                          */
/*     blkno    I      Block number to read                           */
/*     fcb      I(*)   File control block for open map disk file      */
/*                     and buffer involved                            */
/*     nbytes   I      Number of real bytes to be transferred         */
/*  In/out:                                                           */
/*     buff     I(*)   I/O buffer                                     */
/*  Output:                                                           */
/*     ierr     I      Error return code: 0 => no error               */
/*                        2 => bad opcode                             */
/*                        3 => I/O error (also see ZWAI2)             */
/*                        4 => end of file (see ZWAI2)                */
/*  "fcb" has syserr for ZTPWAD to return error                       */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr;
   int fd, n7 = 7;
   off_t firstbyte;
   register int i;
   char op[5], msgbuf[80];
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
                                        /* is 2048.                   */
      i = *nbytes / 2048;
      i = i * 2048;
      if (*nbytes != i) {
         *ierr = 2;
         }
      else {
                                        /* Calculate the byte offset  */
                                        /* (0-relative) in the file   */
                                        /* that correspond to the     */
                                        /* virtual block number.      */
         firstbyte = (*blkno - 1);
         firstbyte = firstbyte * 2048;
                                        /* Set file pointer of the    */
                                        /* file descriptor to the     */
                                        /* absolute byte address      */
                                        /* "firstbyte".               */
         if (lseek (fd, firstbyte, 0) == -1) {
            *(fcb + Z_fcberr) = errno;
            sprintf (msgbuf,
               "ZDKMID: LSEEK ERROR ON FILE DESCRIPTOR %d", fd);
            zmsgwr_ (msgbuf, &n7);
            sprintf (msgbuf,
               "ZDKMID: POSITIONING FILE POINTER TO BYTE OFFSET %d",
               firstbyte);
            zmsgwr_ (msgbuf, &n7);
            *ierr = 3;
            }
                                        /* Perform the I/O and record */
                                        /* the # of bytes transferred */
                                        /* in *(fcb + Z_fcbxfr).      */
                                        /* Record the system error    */
                                        /* code (if any) in           */
                                        /* *(fcb + Z_fcberr).         */
         else {
            if (strcmp (op, "READ") == 0) {
               if ((*(fcb + Z_fcbxfr) =
                  read (fd, buff, *(fcb + Z_fcbreq))) == -1) {
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
      }

   return;
}
