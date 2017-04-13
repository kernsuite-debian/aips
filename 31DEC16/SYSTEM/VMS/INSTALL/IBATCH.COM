$!IBATCH.COM
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
$!---------------------------------------------------------------------
$!  This is the batch version of IBUILD, the second of two main command
$!  procedures for installing AIPS. This procedure should be run after
$!  running ILOAD and after making any necessary changes to the source
$!  code.  This procedure will build all object module libraries,
$!  compile the subroutines, put them into the libraries, and compile
$!  and link all programs.  This batch version does not reinitialize
$!  the data files because an interactive password entry is required
$!  to run program FILAI2 when existing files are written over.
$!  To run this procedure type
$!     $ SUBMIT/NOPRINT/KEEP IBATCH
$!  After the program is finished, do an
$!     $ @IBATFINI
$!  to create the data files.
$!---------------------------------------------------------------------
$!
$ set NoOn
$                                        ! Setup for acting like an
$                                        ! AIPS programmer.
$ @AssnStart.NEW          DEFINE/PROCESS
$ @AIPS_StartUp:AssnBasic DEFINE/PROCESS
$ @AIPS_StartUp:Assnlocal DEFINE/PROCESS
$                                        ! Get values of key instal-
$                                        ! latin variables, such as TV
$                                        ! type.
$ @InstallVar
$!
$ @AIPS_StartUp:AssnProg  DEFINE/PROCESS
$ COMRPL:==@AIPS_PROC:COMRPL
$ COMLNK:==@AIPS_PROC:COMLNK
$ COMTST:==@AIPS_PROC:COMTST
$ VERSION:==DEFINE AIPS_VERSION
$ AIPS:==@AIPS_PROC:AIPS
$                                        ! Collect a copy of all error
$                                        ! messages in file ERROR.DAT
$ open /write ErrFile ERROR.DAT
$ define sys$error ErrFile
$!******************************************************************
$!****** GOTO ALL    ! Rebuild all of AIPS
$ GOTO SOME   ! Redo necessary tasks only
$!******************************************************************
$ SOME:
$    if (TAPEDENS.eqs."6250") then GOTO SOMEOK
$      if (APTYPE.ne.1) then @iReDoAP
$      if (APTYPE.eq.1) then @iReDoAllTV
$      if (APTYPE.ne.1) then @iReDoSomTV
$      purge load:*.exe
$      GOTO FINI
$ SOMEOK:
$    if (APTYPE.ne.1) .and. (APTYPE.ne.3) then @iReDoAP
$    if (APTYPE.eq.3) then copy LOAD1:*.exe LOAD:*
$    if (TVTYPE.ne.1) .and. (APTYPE.eq.1) then @iReDoAllTV
$    if (TVTYPE.ne.1) .and. (APTYPE.ne.1) then @iReDoSomTV
$    purge load:*.exe
$    if (APTYPE.eq.3) then del load1:*.exe;*,load3:*.exe;*
$    GOTO FINI
$ ALL:
$    @iReBuild
$!
$!
$ FINI:
$ close ErrFile
$ deass sys$error
$ EXIT
