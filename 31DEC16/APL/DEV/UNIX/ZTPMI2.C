#define Z_tpmi2__
#include <unistd.h>
#include <string.h>
#include <errno.h>
#include <AIPSPROTO.H>

#if __STDC__
   void ztpmi2_(char *oper, int *fcb, char *buff, int *nbytes, int *ierr)
#else
   void ztpmi2_(oper, fcb, buff, nbytes, ierr)
   char oper[], buff[];
   int *fcb, *nbytes, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! read/write from/to a tape device                                  */
/*# IO-basic Tape                                                     */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2003                                   */
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
/*  Read/write "nbytes" 8-bit bytes of data from/to a tape device     */
/*  using "map" I/O (i.e., double buffered, asynchronous I/O).        */
/*  Inputs:                                                           */
/*     oper     C*4    Operation code "READ" or "WRIT"                */
/*     fcb      I(*)   File control block for opened tape drive       */
/*     buff     I(*)   I/O buffer                                     */
/*     nbytes   I      Number of 8-bit bytes to be transferred        */
/*  Output:                                                           */
/*     ierr     I      Error return code: 0 => no error               */
/*                        2 => bad opcode                             */
/*                        3 => I/O error (left for ZTPWA2 now)        */
/*  Generic UNIX version - sysnchronous I/O actually, reports IO error*/
/*  but not End Of File                                               */
/*  Modified for real-time VLA calling olread when fd < 0             */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   int fd, n8 = 8, ifd, ntry, otry, jerr;
   register int i;
   float secs = 100.0;
   char op[5], msgbuf[80], pathname[MAXAPATH] = "ON-LINE";
/*--------------------------------------------------------------------*/
   *ierr = 0;
   pathname[7] = '\0';
                                        /* Form null terminated       */
                                        /* opcode.                    */
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZTPOP2.    */
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
                                        /* Perform the I/O and record */
                                        /* the # of bytes transferred */
                                        /* in *(fcb + Z_fcbxfr).      */
                                        /* Record the system error    */
                                        /* code (if any) in           */
                                        /* *(fcb + Z_fcberr).         */
      if (strcmp (op, "READ") == 0) {
                                        /* fake fd<0 ==> use Wes'     */
                                        /* on-line device.            */
         if (fd < 0) {
            for (ntry=0; ntry<100; ntry++) {
               *(fcb + Z_fcbxfr) = olread_(&fd, buff, *(fcb + Z_fcbreq));
                                        /* error - close and re-open  */
               if (*(fcb + Z_fcbxfr) == 0) {
                  sprintf (msgbuf, "ZTPMI2: End-of-data: try to reopen");
                  zmsgwr_ (msgbuf, &n8);
                  olclose_(&fd);
                  for (otry=0; otry<100; otry++) {
                     ifd = -99;
                     zdela2_ (&secs, &jerr) ;
                     fd = olopen_(&ifd, pathname, strlen(pathname));
                     sprintf (msgbuf, "ZTPMI2: olopen %d", fd);
                     zmsgwr_ (msgbuf, &n8);
                                        /* open fails */
                     if (fd == -99 || fd >= 0) {
                        sprintf (msgbuf,
                           "ZTPOP2: FAILS TO OPEN ON-LINE - RETRY");
                        if (otry == 99) {
                           sprintf (msgbuf,
                              "ZTPOP2: FAILS TO OPEN ON-LINE - QUIT");
                           ntry = 101;
                           }
                        zmsgwr_ (msgbuf, &n8);
                        }
                     else {
                        *(fcb + Z_fcbfd) = fd;
                        otry = 1000;
                        }
                     }
                  }
               else {
                  ntry=1000;
                  }
               }
            }
         else {
            *(fcb + Z_fcbxfr) = read(fd, buff, *(fcb + Z_fcbreq));
            }
         if (*(fcb + Z_fcbxfr) == -1) {
            *(fcb + Z_fcberr) = errno;
            }
         }
      else if (strcmp (op, "WRIT") == 0) {
         if ((*(fcb + Z_fcbxfr) =
            write (fd, buff, *(fcb + Z_fcbreq))) == -1) {
            *(fcb + Z_fcberr) = errno;
            }
         }
                                        /* Bad opcode.                */
      else {
         *ierr = 2;
         }
      }
                                        /* Exit.                      */
   return;
}
