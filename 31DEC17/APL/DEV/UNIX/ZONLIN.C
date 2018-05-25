#define Z_onlin__
#include <errno.h>
#include <AIPSPROTO.H>

int olerror ()
/*--------------------------------------------------------------------*/
/*! ol_routines for fetching real-time data from shared memory        */
/*# IO-basic Tape Z3                                                  */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995, 1997, 2003-2004                              */
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
/*   olerror - print on on-line stubbed message                       */
/*--------------------------------------------------------------------*/
{
  char msgbuf[80];
  int n8 = 8;

   sprintf (msgbuf, "ONLINE FUNCTION CALLED IN NON-ONLINE VERSION") ;
   zmsgwr_ (msgbuf, &n8) ;
   return(-1);
}

#if __STDC__
   int olopen_(int *unit_no, char *unit_name, int clen)
#else
   int olopen_(unit_no, unit_name, clen)
   int  *unit_no, clen;
   char *unit_name;
#endif
/*--------------------------------------------------------------------*/
/*   olopen  - opens the on online status record                      */
/*   Input/output:                                                    */
/*      unit_no   *I   IO device number assigned (-199 returned here) */
/*                     => not on-line version                         */
/*      unit_name *C   device assignment unused                       */
/*      clen      I    length of unit_name unused                     */
/*   Return:                                                          */
/*      olopen_   I    IO device number assigned                      */
/*   Generic UNIX: stubbed                                            */
/*--------------------------------------------------------------------*/
{
   olerror;
   errno = 5;
   *unit_no = -199;
   return(*unit_no);
}

#if ___STDC__
   int olread_(int *unit_no, char *buffer, int buff_len)
#else
   int olread_(unit_no, buffer, buff_len)
   int  *unit_no;
   char *buffer;
   int   buff_len;
#endif
/*--------------------------------------------------------------------*/
/*   olread  - returns a data from the shared memory                  */
/*   Input:                                                           */
/*      unit_no   *I   IO device number assigned                      */
/*      buff_len  I    max size of buffer unused                      */
/*   Output:                                                          */
/*      buffer    *C   data buffer                                    */
/*   Return:                                                          */
/*      olread_   I    length of buffer read, -1 => EOF               */
/*   Generic UNIX: stubbed                                            */
/*--------------------------------------------------------------------*/
{
   olerror;
   errno = 5;
   return (-1);
}

#if __STDC__
   int olclose_(int *unit_no)
#else
   int olclose_(unit_no)
   int *unit_no;
#endif
/*--------------------------------------------------------------------*/
/*   olclose - close out the online status record                     */
/*   Input:                                                           */
/*      unit_no   *I   IO device number assigned                      */
/*   Return:                                                          */
/*      olclose_   I   0                                              */
/*   Generic UNIX: stubbed                                            */
/*--------------------------------------------------------------------*/
{
   olerror;
   return(0);
}

#if __STDC__
   int oladvf_(int *unit_no, int *files)
#else
   int oladvf_(unit_no, files)
   int *unit_no, *files;
#endif
/*--------------------------------------------------------------------*/
/*   oladvf - advance file for online disk                            */
/*   Input:                                                           */
/*      unit_no   *I   IO device number assigned                      */
/*      files     *I   IO device number assigned                      */
/*                     < 0 -> start of yesterday's file               */
/*                     = 0 -> start of today's file                   */
/*                     > 0 -> current end of today's file             */
/*   Return:                                                          */
/*      oladvf_    I   -1 -> illegal call                             */
/*                     0 -> okay, 1 -> start today not yesterday      */
/*   Generic UNIX: stubbed                                            */
/*--------------------------------------------------------------------*/
{
  olerror;
   return(-1);
}

