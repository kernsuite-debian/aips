$!  IBUILD.COM
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
$!  This is the second of two main command procedures for installing
$!  AIPS. This procedure should be run after running ILOAD and after
$!  making any necessary changes to the source code.  This procedure
$!  will build all object module libraries, compile the subroutines,
$!  put them into the libraries, and compile and link all programs.
$!  Finally all the data files needed for AIPS are created.
$!  This procedure should be run with the default directory set to the
$!  directory corresponding to [AIPS] and the default disk set to
$!  the AIPS areas' default disk.  To run this procedure type @IBUILD.
$!---------------------------------------------------------------------
$!
$ write sys$output "Making logical assignments (Takes about 1 min.)"
$ write sys$output " "
$                                   ! Get values of key instal-
$                                   ! latin variables, such as TV
$                                   ! type.
$ set NoOn
$                                   ! Setup for acting like an
$                                   ! AIPS programmer.
$ @AssnStart.NEW          DEFINE/PROCESS
$ @AIPS_StartUp:AssnBasic DEFINE/PROCESS
$ @AIPS_StartUp:Assnlocal DEFINE/PROCESS
$ @InstallVar
$ @AIPS_StartUp:AssnProg  DEFINE/PROCESS
$ COMRPL:==@AIPS_PROC:COMRPL
$ COMLNK:==@AIPS_PROC:COMLNK
$ COMTST:==@AIPS_PROC:COMTST
$ VERSION:==DEFINE AIPS_VERSION
$ AIPS:==@AIPS_PROC:AIPS
$                                   ! Collect a copy of all error
$                                   ! messages in file ERROR.DAT
$ open /write ErrFile ERROR.DAT
$ define sys$error ErrFile
$ write sys$output " "
$ write sys$output " "
$ QUEST:
$    inquire What "1=Rebuild all of AIPS.  2=Redo necessary tasks only."
$    if (What.eq.1) then GOTO ALL
$    if (What.eq.2) then GOTO SOME
$ GOTO QUEST
$ SOME:
$    if (TAPEDENS.eqs."6250") then GOTO SOMEOK
$      if (APTYPE.ne.1) then @iReDoAP
$      if (APTYPE.eq.1) then @iReDoAllTV
$      if (APTYPE.ne.1) then @iReDoSomTV
$      purge load:*.exe
$      GOTO INITFILES
$ SOMEOK:
$    if (APTYPE.ne.1) .and. (APTYPE.ne.3) then @iReDoAP
$    if (APTYPE.eq.3) then copy LOAD1:*.exe LOAD:*
$    if (TVTYPE.ne.1) .and. (APTYPE.eq.1) then @iReDoAllTV
$    if (TVTYPE.ne.1) .and. (APTYPE.ne.1) then @iReDoSomTV
$    purge load:*.exe
$    if (APTYPE.eq.3) then del load1:*.exe;*,load3:*.exe;*
$    GOTO INITFILES
$ ALL:
$    @iReBuild
$!
$ INITFILES:
$ close ErrFile
$ deass sys$error
$!
$                                   ! Run file builder/init program that
$                                   ! reads inputs from file SYSPARM.
$     WRITE SYS$OUTPUT "BUILDING/INITIALIZING DATA FILES"
$     write sys$output -
        "Setting default file protections to (S:RWED,O:RWED,G:RWE,W:RWE)"
$     SET protection=(S:RWED,O:RWED,G:RWE,W:RWE) /DEFAULT
$     run load:FILAI2
$!
$                                   ! Run POPSGN to initialize memory
$                                   ! (ME) files.
$     write sys$output "INITIALIZING MAIN MEMORY FILES."
$     write sys$output "ignore NOLOGNAM message if it occurs."
$     deass FOR005
$     write sys$output "ignore '>' when it appears."
$     run LOAD:POPSGN
1 15 0 POPSDAT ' '
<CR>
$!
$ EXIT
