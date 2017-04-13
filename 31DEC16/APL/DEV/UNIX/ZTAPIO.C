#define Z_tapio__
#include <string.h>
#include <unistd.h>
#include <fcntl.h>
#include <errno.h>

#if __STDC__
   void ztapio_(char *oper, int *ln, char *name, int *fd, int *bytreq,
      int *bytread, char *buf, int *syserr)
#else
   void ztapio_(oper, ln, name, fd, bytreq, bytread, buf, syserr)
   int *ln, *fd, *bytreq, *bytread, *syserr;
   char oper[], name[], buf[];
#endif
/*--------------------------------------------------------------------*/
/*! tape operations for IMPFIT (compressed FITS transport tape)       */
/*# Tape                                                              */
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
/*  tape operations for IMPFIT (compressed FITS transport tape)       */
/*  Inputs:                                                           */
/*     oper     C*4    'OPEN', 'READ', 'CLOS'                         */
/*     name     C*(*)  physical file name                             */
/*     bytreq   I      bytes to be read                               */
/*     ln       I      length of name                                 */
/*  In/out:                                                           */
/*     fd       I(*)   file descriptor (set on OPEN, else used)       */
/*  Output:                                                           */
/*     bytread  I      Bytes read on READ                             */
/*     buf      I(*)   Data buffer read                               */
/*     syserr   I      System error code                              */
/*  Generic UNIX version.                                             */
/*--------------------------------------------------------------------*/
{
   char nam[65], op[5];
   int i;
/*--------------------------------------------------------------------*/
                                        /* get operation */
   for (i=0; (i < 4) && (oper[i] != ' '); i++)
      op[i] = oper[i];
   op[i] = '\0';

   errno = 0 ;
   if (strcmp (op, "READ") == 0) {
      *bytread = read(*fd,buf,*bytreq);
      }

   else if (strcmp (op, "OPEN") == 0) {
      for (i=0; (i < *ln) && (name[i] != ' ') ; i++)
         nam[i] = name[i];
      nam[i] = '\0';
      *fd = open (nam, 0);
      }

   else if (strcmp (op, "CLOS") == 0) {
      close (*fd);
      }

   *syserr = errno ;

   return;
}
