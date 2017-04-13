$! Procedure to  write a FITS tape containing the current version
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
$!    of AIPS. DCW, 1Dec82-25July83; EWG 9Apr86.
$!
$!  Use this procedure only for the first execution of EXPFIT for
$!  a tape/version.  To restart a failed EXPFIT, delete the highest
$!  version only of AIPSTAPEnnn.LIS and run EXPFIT2.COM, where nnn is
$!  the tape file number.  Also delete the highest EXPFITnnn.TXT.
$!
$!  requires tape unit name as argument and AIPS version as 2nd arg.
$!      i.e.   @EXPFIT MMA0: 15JUL86
$!
$ Set Verify
$ Ass 'P1' aipstape
$ Mount/For/Blo=32767/density=1600 AIPSTAPE:
$ dt :== dir/col=1/nohead/notrail/vers=1/out=DIRECTORY.DAT-
/excl=(*.obj,*.map,*.lis,*.tmp,me%0000*.,*.ml,*.cpk,*.dir,*.lib,*.prt,-
*.exe,*.err,*.sav,*.old,*.jnk,*.chk,*.olb,*.upd,*.log,*.src,_$*.*)
$ del/log DIRECTORY.DAT;*,AIPSTAPE*.LIS;1,EXPFIT*.TXT;1
$ dt uma0:[AIPS.'P2'...]IMPFIT.FOR,uma0:[AIPS.'P2'...]*.*, -
     uma0:[aips.text.publ]*.*,uma0:[aips.text.ions]*.*
$ Run LOAD:EXPFIT
$ purge/log AIPSTAPE.LIS,EXPFIT.TXT
$ dismount AIPSTAPE:
$ Set NoVerify
