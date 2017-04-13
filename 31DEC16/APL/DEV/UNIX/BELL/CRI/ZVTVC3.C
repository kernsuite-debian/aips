#include <errno.h>

zvtvc3_(fcb, ierr)
/*--------------------------------------------------------------------*/
/*! close connection in real-TV computer to client, virtual-TV comp.  */
/*# TV-IO                                                             */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1995                                               */
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
/*  ZVTVC3 closes the connection in TVMON to the remote machine which */
/*  is running the AIPS VTV (Virtual TV) code.                        */
/*  Inputs:                                                           */
/*     fcb   I(*)   File descriptor                                   */
/*  Output:                                                           */
/*     ierr  I      Error: 0 => okay                                  */
/* UNICOS version.                                                    */
/* deassigns via 'close' certain devices i.e. TV socket               */
/*--------------------------------------------------------------------*/
int *fcb, *ierr;
/*--------------------------------------------------------------------*/
{
                                        /* Offsets to entries in the  */
                                        /* file control blocks        */
   extern int Z_fcbfd, Z_fcbreq, Z_fcberr, Z_fcbxfr, Z_fcbsiz,
      Z_fcbsp1, Z_fcbsp2, Z_fcbsp3, Z_fcbsp4;
   extern int errno;
   int sfd, cfd;
/*--------------------------------------------------------------------*/
   *ierr = 0;

   sfd = *(fcb + Z_fcbfd) ;
   cfd = *(fcb + Z_fcbsp1) ;

   if (shutdown(cfd, 0) != 0) {
      *(fcb + Z_fcberr) = errno;
      *ierr += 8;
      }
   if (close(cfd) != 0) {
      *(fcb + Z_fcberr) = errno;
      *ierr += 4;
      }

   if (shutdown(sfd, 0) != 0) {
      *(fcb + Z_fcberr) = errno;
      *ierr += 2;
      }
   if (close(sfd) != 0) {
      *(fcb + Z_fcberr) = errno;
      *ierr += 1;
      }

   return;
}
