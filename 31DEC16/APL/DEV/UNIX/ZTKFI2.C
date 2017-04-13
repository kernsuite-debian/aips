#define Z_tkfi2__
#include <unistd.h>
#include <string.h>
#include <errno.h>

#if __STDC__
   void ztkfi2_(char *oper, int *fcb, char *buff, int *nbytes,
      int *ierr)
#else
   void ztkfi2_(oper, fcb, buff, nbytes, ierr)
   char oper[], buff[];
   int *fcb, *nbytes, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! read/write from/to a Tektronix device                             */
/*# Graphics                                                          */
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
/*  Read/write "nbytes" of data from/to a Tektronix terminal.         */
/*                                                                    */
/*  Inputs:                                                           */
/*     oper     C*4    Operation code "READ" or "WRIT"                */
/*     fcb      I(*)   File control block for opened Tektronix        */
/*     buff     I(*)   I/O buffer                                     */
/*     nbytes   I      Number of 8-bit bytes to be transferred        */
/*  Output:                                                           */
/*     ierr     I      Error return code: 0 => no error               */
/*                        2 => bad opcode                             */
/*                        3 => I/O error                              */
/*                        4 => end of file                            */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   int fd;
   register int i;
   char op[5];
/*--------------------------------------------------------------------*/
   *ierr = 0;
                                        /* Form null terminated       */
                                        /* opcode.                    */
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZTKOP2.    */
   fd = *(fcb + Z_fcbfd);
                                        /* Record byte request in the */
                                        /* file control block (ZTKBUF */
                                        /* suggests that "nbytes" is  */
                                        /* in units of 8-bit bytes).  */
   *(fcb + Z_fcbreq) = *nbytes;
                                        /* Also, zero error status &  */
                                        /* transfer count entries.    */
   *(fcb + Z_fcberr) = 0;
   *(fcb + Z_fcbxfr) = 0;
                                        /* Perform the I/O and record */
                                        /* the # of bytes transferred */
                                        /* in *(fcb + Z_fcbxfr).      */
                                        /* Record the system error    */
                                        /* code (if any) in           */
                                        /* *(fcb + Z_fcberr).         */
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
      return;
      }
                                        /* End of file?               */
   if (*(fcb + Z_fcbxfr) == 0)
      *ierr = 4;

   return;
}
