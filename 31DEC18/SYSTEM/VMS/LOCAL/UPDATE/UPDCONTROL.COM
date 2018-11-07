$! UPDCONTROL.COM
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
$! Copy all CVAX control files containing lists of modified files,
$! sort them and get rid of duplicates and entries already processed.
$! Signals:
$!   $status
$!-----------------------------------------------------------------------
$ Write Sys$Output "*** UPDCONTROL starting"
$ define/nolog sys$error updcontrol.err
$ Write Sys$Output "*** Error output diverted to UPDCONTROL.ERR"
$                                ! Copy all control files.
$ @UpdVms:updcopysub cvax::uma0:[aips.'VerDate'.update]comrpl.upd  comrpl.upd
$ @UpdVms:updcopysub cvax::uma0:[aips.'VerDate'.update]comlnk.upd  comlnk.upd
$ @UpdVms:updcopysub cvax::uma0:[aips.'VerDate'.update]putbck.upd  putbck.upd
$ @UpdVms:updcopysub cvax::uma0:[aips.'VerDate'.update]reasons.upd reasons.upd
$ @UpdVms:updcopysub cvax::uma0:[aips.'VerDate'.update]remove.upd  remove.upd
$ Write Sys$output "*** UPDCONTROL finished copying, start sorting"
$                                ! Sort Them
$ @UpdVms:updsrtunq comrpl
$ @UpdVms:updsrtunq comlnk
$ @UpdVms:updsrtunq putbck
$ @UpdVms:updsrtunq reasons
$ @UpdVms:updsrtunq remove
$!
$ Write sys$output "*** UPDCONTROL finished."
$ EXIT
