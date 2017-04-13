#define Z_ttyio__
#include <unistd.h>
#include <string.h>
#include <stdio.h>
#include <errno.h>
#include <sys/types.h>
#include <AIPSPROTO.H>

#if __STDC__
   void zttyi2_(char *oper, int *fcb, char *buff, int *nbytes,
                int *ierr)
#else
   void zttyi2_(oper, fcb, buff, nbytes, ierr)
   char oper[], buff[];
   int *fcb, *nbytes, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! read/write from/to a Terminal device                              */
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
/*  Read/write "nbytes" of data from/to a message server socket.      */
/*                                                                    */
/*  Inputs:                                                           */
/*     oper     C*4    Operation code "READ" (unsupported) or "WRIT"  */
/*     fcb      I(*)   File control block for opened message terminal */
/*     buff     I(*)   I/O buffer                                     */
/*     nbytes   I      Number of 8-bit bytes to be transferred        */
/*  Output:                                                           */
/*     ierr     I      Error return code: 0 => no error               */
/*                        2 => bad opcode                             */
/*                        3 => I/O error                              */
/*                        4 => end of file                            */
/*                        5 => error reading handshake from socket    */
/*                        6 => handshake did not say "OK"             */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4, Z_nfcbfd, Z_nfcber;

   extern int remembered;
   int fd, ndone, nread, llen, tlen, jerr;
   register int i;
   char op[5], lognam[6], tname[8], ok[3];
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
   fd = *(fcb + Z_nfcbfd);
                                        /* Also, zero error status    */
   *(fcb + Z_nfcber) = 0;
                                        /* Perform the I/O and record */
                                        /* the system error code (if  */
                                        /* any) in *(fcb + Z_nfcber). */
   if (strcmp (op, "READ") == 0) {
                                        /* This code never used, and  */
                                        /* CERTAINLY never tested!    */
      if ((ndone=read (fd, buff, *nbytes)) == -1) {
         *(fcb + Z_nfcber) = errno;
         *ierr = 3;
         }
      }
   else if (strcmp (op, "WRIT") == 0) {
                                        /* write to message server    */
      if ((ndone=write (fd, buff, *nbytes)) == -1) {
         *(fcb + Z_nfcber) = errno;
         *ierr = 3;
	 perror ("ZTTYI2: write to msgserver");
         }
      else {
                                        /* Read the "OK" back         */
	 ndone = 2;
	 if (( nread = read (fd, ok, ndone)) == -1) {
	    *ierr = 5;
	    fprintf (stderr, "ZTTYI2: socket from msgserver closed!\n");
            }
	 else if (nread == 0) {
	    *ierr = 5;
	    fprintf (stderr, "ZTTYI2: msgserver sent back NULL!\n");
            }
	 else if (nread != 2) {
	    *ierr = 5;
	    fprintf (stderr, "ZTTYI2: msgserver sent %d bytes != 2\n",
		     nread);
            }
	 else {
	    if (strncmp (ok, "OK", 2) != 0) {
	       *ierr = 6;
	       fprintf (stderr,
			"ZTTYI2: unexpected msgserver text %s\n", ok);
	       }
            }
         }
      if (*ierr != 0) {
                                        /* problems, forget msgserver */
                                        /* but print possibly lost msg */
         fprintf (stderr, "ZTTYI2: Problem with MSGSRV communication\n");
	 fprintf (stderr, "ZTTYI2: %s\n",
		  "Messages will continue here instead; next may be a repeat");
         fprintf (stderr, "%s\n", buff);
	 remembered = 0;
	 llen=5;
	 strcpy (lognam, "TTDEV");
	 tlen=7;
	 strcpy (tname, "TTDEV00");
	 zcrlog_ (&llen, lognam, &tlen, tname, &jerr);
	 if (jerr != 0) {
	    fprintf (stderr,
		     "ZTTYI2: (info) problem writing to msgserver\n");
	    fprintf (stderr,
		     "ZTTYI2: (info) failed to reset message device\n");
            }
         }
      }
                                        /* Bad opcode.                */
   else {
      *ierr = 2;
      return;
      }
                                        /* End of file?               */
   if (ndone == 0)
      *ierr = 4;

   return;
}
