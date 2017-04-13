$! VAX3.COM
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
$!----------------------------------------------------------------------
$! Copy HELP files and executable modules from node AIPS to VAX3.
$!----------------------------------------------------------------------
$ write sys$output "*** VAX3 starting"
$ define/nolog sys$error vax3.err
$ Write sys$output "*** Error output diverted to VAX3.ERR"
$!
$ DateTime = f$time()
$ if F$Extract(0,1,DateTime).EQS." " then DateTime[0,1] := "0"
$ on ERROR then GOTO ERROREXIT
$ open/read lastgood vax3::disk$res:[aips.'VerDate'.update]lastgood.dat
$ read lastgood vax3date
$ close lastgood
$ Write sys$Output "*** Last good date on VAX3 was ",vax3date
$ vax3date[11,1] := ":"
$!
$ define/user sys$output vax3hlp.log
$ copy/log/since='vax3date' disk$res:[aips.'VerDate'.help]*.hlp -
   vax3::disk$res:[aips.'VerDate'.help]*.*
$ set NoOn
$ purge vax3::disk$res:[aips.'VerDate'.help]*.*
$ set On
$ Write sys$output "*** HLP files copied to VAX3 and purged there"
$ on ERROR then GOTO ERROREXIT
$ define/user sys$output vax3load.log
$ copy/log/since='vax3date' disk$res:[aips.'VerDate'.load]*.exe -
   vax3::disk$res:[aips.'VerDate'.load]*.*
$ set NoOn
$ purge vax3::disk$res:[aips.'VerDate'.load]*.*
$ Write sys$output "*** Load files copied to VAX3 and purged there"
$ set On
$ on ERROR then GOTO ERROREXIT
$ define/user sys$output vax3alt1.log
$ copy/log/since='vax3date' disk$res:[aips.'VerDate'.load.alt1]*.exe -
   vax3::disk$res:[aips.'VerDate'.load.alt1]*.*
$ set NoOn
$ purge vax3::disk$res:[aips.'VerDate'.load.alt1]*.*
$ set On
$ Write sys$output "*** Load (ALT1) files copied to VAX3 and purged there"
$ on ERROR then GOTO ERROREXIT
$ define/user sys$output vax3sys.log
$ copy/log/since='vax3date' disk$res:[aips.'VerDate'.run]*.*; -
   vax3::disk$res:[aips.'VerDate'.run]*.*
$ copy/log/since='vax3date' disk$res:[aips.'VerDate'.system]*.*; -
   vax3::disk$res:[aips.'VerDate'.system]*.*
$ copy/log/since='vax3date' disk$res:[aips.'VerDate'.system.vms]*.*; -
   vax3::disk$res:[aips.'VerDate'.system.vms]*.*
$ copy/log/since='vax3date' disk$res:[aips.'VerDate'.system.vms.update]*.*; -
   vax3::disk$res:[aips.'VerDate'.system.vms.update]*.*
$ copy/log/since='vax3date' -
   disk$res:[aips.'VerDate'.system.vms.update.vla]*.*; -
   vax3::disk$res:[aips.'VerDate'.system.vms.update.vla]*.*
$ copy/log/since='vax3date' disk$res:[aips.'VerDate'.system.vms.local]*.*; -
   vax3::disk$res:[aips.'VerDate'.system.vms.local]*.*
$ set NoOn
$ purge vax3::disk$res:[aips.'VerDate'.run]*.*
$ purge vax3::disk$res:[aips.'VerDate'.system]*.*
$ purge vax3::disk$res:[aips.'VerDate'.system.vms]*.*
$ purge vax3::disk$res:[aips.'VerDate'.system.vms.local]*.*
$ purge vax3::disk$res:[aips.'VerDate'.system.vms.update]*.*
$ purge vax3::disk$res:[aips.'VerDate'.system.vms.update.vla]*.*
$ set On
$ Write sys$output "*** RUN files copied to VAX3 and purged there"
$ on ERROR then GOTO ERROREXIT
$ open/write lastgood vax3::disk$res:[aips.'VerDate'.update]lastgood.dat
$ write lastgood DateTime
$ close lastgood
$ Write sys$output "*** VAX3 finished; VAX3 lastgood is now ",DateTime
$ EXIT
$!
$ ERROREXIT:
$   write updlogfile "*** ERROR ** Updating VAX3"
$   Write sys$Output "*** VAX3 finished with error(s)."
$   EXIT %X1000002C
