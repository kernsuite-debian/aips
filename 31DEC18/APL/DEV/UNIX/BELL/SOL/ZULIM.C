#define Z_ulim__
#include <sys/time.h>
#include <sys/resource.h>
#include <unistd.h>

#if __STDC__
   void zulim_ (int *nfiles)
#else
   zulim_ (nfiles)
   int *nfiles;
#endif
/*--------------------------------------------------------------------*/
/*! change system limit to allow more open file                       */
/*# Z Z2 System                                                       */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 1996, 1998                                               */
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
/*  Call the OS to allow the process to open at least NFILES files at */
/*  the same time.  Errors are ignored in the hope that the process   */
/*  will not reach the limit that we were unable to change.           */
/*                                                                    */
/*  Only actually honors requests to increase the max number of open  */
/*  files; requests to decrease the max return silently, with no      */
/*  change having been performed.                                     */
/*                                                                    */
/*  Inputs:                                                           */
/*     NFILES   I   Maximum number of open files this process needs   */
/*  Outputs:                                                          */
/*     none                                                           */
/*                                                                    */
/*  Solaris UNIX version                                              */
/*  JAU 06 May 96: Jeff Uphoff, NRAO.                                 */
/*--------------------------------------------------------------------*/
{
   struct rlimit rlp;
/*--------------------------------------------------------------------*/
   getrlimit (RLIMIT_NOFILE, &rlp);     /* Get current limits.        */

                                        /* Check if increase.         */
   if ((rlim_t)*nfiles > rlp.rlim_cur) {
      rlp.rlim_cur = (rlim_t)*nfiles;   /* Set new limit if so.       */
      setrlimit (RLIMIT_NOFILE, &rlp);
   }
   return;
}

/*
 * Local Variables:
 * c-basic-offset: 3
 * comment-column: 40
 * indent-tabs-mode: nil
 * End:
 */
