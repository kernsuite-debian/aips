#define Z_exit__
#include <stdlib.h>

#if __STDC__
   void zexit_ (int *a)
#else
   void zexit_ (a)
   int *a;
#endif
/*--------------------------------------------------------------------*/
/*! does task termination with numeric code                           */
/*# Service                                                           */
/*--------------------------------------------------------------------*/
/*;  Copyright (C) 2012                                               */
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
/*   Task termination returning status code to calling procedure      */
/*   Inputs:                                                          */
/*      a      I    status code: 0 no error, > 0 error                */
/*   Replaces CALL EXIT(n) in old FORTRANs                            */
/*   Suggested values <sysexits.h> are                                */
/*      EX_USAGE        64  command line usage error                  */
/*      EX_DATAERR      65  data format error                         */
/*      EX_NOINPUT      66  cannot open input                         */
/*      EX_NOUSER       67  addressee unknown                         */
/*      EX_NOHOST       68  host name unknown                         */
/*      EX_UNAVAILABLE  69  service unavailable                       */
/*      EX_SOFTWARE     70  internal software error                   */
/*      EX_OSERR        71  system error (e.g., can't fork)           */
/*      EX_OSFILE       72  critical OS file missing                  */
/*      EX_CANTCREAT    73  can't create (user) output file           */
/*      EX_IOERR        74  input/output error                        */
/*      EX_TEMPFAIL     75  temp failure; user is invited to retry    */
/*      EX_PROTOCOL     76  remote error in protocol                  */
/*      EX_NOPERM       77  permission denied                         */
/*      EX_CONFIG       78  configuration error                       */
/*   but use anything > 0 for failures                                */
/*--------------------------------------------------------------------*/
{
/*--------------------------------------------------------------------*/

   exit (*a) ;
   return ;
}
