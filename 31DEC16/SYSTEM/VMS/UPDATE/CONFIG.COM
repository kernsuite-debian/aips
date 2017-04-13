$! CONFIG.COM
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
$! Define the 'configuration' of the UPDATE package at a remote site,
$! selecting which file transfer mode is to be used, which version of
$! that mode, and specifying certain directory paths and network routing
$! paths. DCW, NRAO-CV, 17July87.
$!----------------------------------------------------------------------
$!
$! Update Protocol to be Used (DECNET, FTP or EMAIL):
$ Updprotocol   :==     "DECNET"
$ Updprotocol   :==     "EMAIL"
$!
$! Disk drive on CVAX where AIPS directories are stored:
$ Cvaxdev       :==     "UMA0:"
$ Cvaxaipsroot  :==     "[AIPS.]"
$!
$! Local directory for staging batch file copies:
$ Updtempdir    :==     "USR$ROOT3:[AIPS.TMP]"
$ Updtempdir    :==     "TST:[UPDATE]"
$!
$! Internal names for the files being copied and the file-list:
$ Updfilelist   :==     "''Updtempdir'UFL"
$ Updfilename   :==     "U"
$!
$ Goto  'Updprotocol'
$!
$ DECNET:
$ Nraocvax      :==     "6654::"
$ Goto Quit
$!
$ FTP:
$ Nraocvax      :==     "nrao-cvax.arpa"
$! FTP type can be EXOS, CMU, WIN, ...:
$ Ftt           :==     "EXOS"
$ Goto Quit
$!
$ EMAIL:
$ Nraocvax      :==     "AIPSSERV@NRAO"
$! EMAIL type can be EXOS, JNET, ANL, ...:
$ Ftt           :==     "JNET"
$ Goto Quit
$!
$ Quit:
