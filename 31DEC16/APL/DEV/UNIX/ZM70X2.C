#define Z_m70x2__
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>
#include <AIPSPROTO.H>

#define  NONE 0                         /* Have not yet done r/w      */
#define  READ 1
#define  WRITE 2
#define  ILLEGAL 3
static char *op_names[] = {"NONE","READ","WRITE","ILLEGAL"};
#define MAX_BAD 5                       /* Maximum number of bad xfers*/
                                        /* that we lie about and say  */
                                        /* succeeds.                  */

#define DEBUG_LEVEL 1                   /* 1 for debugging info, 2 for*/
                                        /* Copious output, 0 for none */
                                        /* Undef should generate dead */
                                        /* code for the compiler to   */
                                        /* take out.                  */

                                        /* For debugging use an int   */
                                        /* to set the level so we can */
                                        /* change it on the fly with a*/
                                        /* debugger.                  */
#ifdef DEBUG_LEVEL
static int debug = DEBUG_LEVEL;
#define DEBUG if (debug) printf
#define LOG if (debug>1) printf
#else
#define DEBUG if (0) printf
#define LOG if (0) printf
#endif /* DEBUG_LEVEL */

#if __STDC__
   void zm70x2_(char *oper, int *fcb, char *buff, int *nbytes,
      int *ierr)
#else
   void zm70x2_(oper, fcb, buff, nbytes, ierr)
   char oper[], buff[];
   int *fcb, *nbytes, *ierr;
#endif
/*--------------------------------------------------------------------*/
/*! read/write from/to IIS Model 70/75 device                         */
/*# TV-IO                                                             */
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
/*  Transfer "nbytes" of data between an IIS Model 70 TV device.      */
/*  Inputs:                                                           */
/*     oper     C*4    Operation code "READ" or "WRIT"                */
/*     fcb      I(*)   File control block for opened TV device        */
/*     nbytes   I      Number of 8-bit bytes to be transferred        */
/*  In/out:                                                           */
/*     buff     I(*)   I/O buffer                                     */
/*  Output:                                                           */
/*     ierr     I*2    Error return code: 0 => no error               */
/*                        2 => bad opcode                             */
/*                        3 => I/O error                              */
/*                        4 => end of file                            */
/*  Generic UNIX version - assumes 'read' and 'write' will be okay    */
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;

   int fd;
   register int i;
   char op[5];
   static int last_op = NONE;           /* Keep track of statistics   */
   static int call_number = 0;
   static int n_bad = 0;
   int call_type;
/*--------------------------------------------------------------------*/
   call_number++;
   *ierr = 0;
                                        /* Form null terminated       */
                                        /* opcode.                    */
   for (i = 0; i < 4; i++)
      op[i] = oper[i];
   op[4] = '\0';
                                        /* Get file descriptor from   */
                                        /* the file control block as  */
                                        /* stored there by ZM70O2.    */
   fd = *(fcb + Z_fcbfd);
                                        /* Record byte request in the */
                                        /* file control block.        */
   *(fcb + Z_fcbreq) = *nbytes;
                                        /* Also, zero error status &  */
                                        /* transfer count entries.    */
   *(fcb + Z_fcberr) = 0;
   *(fcb + Z_fcbxfr) = 0;
   LOG ("ZM70X2 enter: call=%d op=%s last=%s nbytes=%d\n",
      call_number, op, op_names[last_op], *nbytes);
                                        /* Zero byte request okay.    */
                                        /* Just return.               */
   if (n_bad && *nbytes < 16) {
      n_bad++;
      DEBUG("ZM70X2: *nbytes=%d, THROW AWAY, nbad=%d\n",*nbytes,n_bad);
      return;
      }
   if (*(fcb + Z_fcbreq) != 0) {
      if (*nbytes == 0)
         DEBUG ("ZM70X2 0 byte return: call=%d\n", call_number);
                                        /* Perform the I/O and record */
                                        /* the # of bytes transferred */
                                        /* in *(fcb + Z_fcbxfr).      */
                                        /* Record the system error    */
                                        /* code (if any) in           */
                                        /* *(fcb + Z_fcberr).         */
      if (strcmp (op, "READ") == 0) {
         call_type = READ;
         if ((*(fcb + Z_fcbxfr) = read (fd, buff, *(fcb + Z_fcbreq)))
            != *nbytes) {
           DEBUG("ZM70X2 read: FAILS call=%d op=%s last=%s nbytes=%d transferred = %d",
              call_number, op, op_names[last_op], *nbytes, *(fcb + Z_fcbxfr));
            n_bad++;
            *(fcb + Z_fcberr) = errno;
            *ierr = 3;
            }
         }
      else if (strcmp (op, "WRIT") == 0) {
         call_type = WRITE;
         if ((*(fcb + Z_fcbxfr) = write (fd, buff, *(fcb + Z_fcbreq)))
            != *nbytes) {
            DEBUG("ZM70X2 write: FAILS call=%d op=%s last=%s nbytes=%d transferred = %d",
               call_number, op, op_names[last_op], *nbytes, *(fcb + Z_fcbxfr));
            n_bad++;
            *(fcb + Z_fcberr) = errno;
            *ierr = 3;
            }
         }
                                        /* Bad opcode.                */
      else {
         *ierr = 2;
         last_op = ILLEGAL;
         DEBUG("ZM70X2 leave: FAILS call=%d, ILLEGAL opcode\n",call_number);
         return;
         }
                                        /* End of file?               */
      if (*(fcb + Z_fcbxfr) == 0)
         *ierr = 4;
      }
   if (*ierr == 0) {
      n_bad = 0;
      }
   else if (n_bad < MAX_BAD) {
      DEBUG("ZM70X2: LIE - turn FAIL into succeed, n_bad=%d\n, try MC",n_bad);
      *(fcb + Z_fcberr) = 0;
      *ierr = 0;
      zm70m2_(fcb, ierr);
      if (*ierr) {
         DEBUG("ZM70X2: Master CLEAR FAILS\n");
         }
      }
   last_op = call_type;
   LOG("ZM70X2 leave: normal, call=%d, ierr=%d\n",call_number,*ierr);

   return;
}
