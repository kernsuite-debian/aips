$! CLEANUP.COM
$!----------------------------------------------------------------------
$!;  Copyright (C) 1995
$!;  Associated Universities, Inc. Washington DC, USA.
$!;
$!;  This program is free software; you can redistribute it and/or
$!;  modify it under the terms of the GNU General Public License as
$!;  published by the Free Software Foundation; either version 2 of
$!;  the License, or (at your option) any later version.
$!;
$!;  This program is distributed in the hope that it will be useful,
$!;  but WITHOUT ANY WARRANTY; without even the implied warranty of
$!;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
$!;  GNU General Public License for more details.
$!;
$!;  You should have received a copy of the GNU General Public
$!;  License along with this program; if not, write to the Free
$!;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
$!;  MA 02139, USA.
$!;
$!;  Correspondence concerning AIPS should be addressed as follows:
$!;         Internet email: aipsmail@nrao.edu.
$!;         Postal address: AIPS Project Office
$!;                         National Radio Astronomy Observatory
$!;                         520 Edgemont Road
$!;                         Charlottesville, VA 22903-2475 USA
$!----------------------------------------------------------------------
$!-----------------------------------------------------------------------
$! Delete any old version of control files, error files, etc.
$!-----------------------------------------------------------------------
$!
$ write sys$output "*** CLEANUP starting"
$ define/nolog sys$error cleanup.err
$ write sys$output "*** Error messages diverted to CLEANUP.ERR"
$ Set NoOn
$ direk = "Aips_Version:[Update]"
$ if f$search("''direk'*.Upd;*") .nes. "" then Delete/log 'direk'*.Upd;*
$ if f$search("''direk'*.Srt;*") .nes. "" then Delete/log 'direk'*.Srt;*
$ if f$search("''direk'*.Unq;*") .nes. "" then Delete/log 'direk'*.Unq;*
$ if f$search("''direk'*.Err;*") .nes. "" then Delete/log 'direk'*.Err;*
$ if f$search("''direk'vax3*.log;*") .nes. "" then Delete/log 'direk'vax3*.log;*
$ if f$search("''direk'*.log;*") .nes. "" then Purge/log  'direk'*.log
$!
$ Purge /keep=3 aips_version:[update]last*.dat
$ Purge /keep=5 aips_version:[update]lastgood.tmp
$!                       ! create lastgood file to copy
$ open /write lastgood aips_version:[update]lastgood.tmp
$                        ! Round down to nearest day so we don't have
$                        ! to worry about things changed during update.
$ DateTime = f$extract(0,12,StartTime) + "00:00:00.00"
$ write lastgood DateTime
$ close lastgood
$ Write sys$output "*** wrote Lastgood.Tmp"
$!
$ Write sys$output "*** End of CLEANUP"
$ EXIT
