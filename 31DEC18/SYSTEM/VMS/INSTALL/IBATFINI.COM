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
$!  This is the second of two main command procedures for installing
$!  AIPS. This procedure should be run after running ILOAD and after
$!  making any necessary changes to the source code.  This procedure
$!  will build all object module libraries, compile the subroutines,
$!  put them into the libraries, and compile and link all programs.
$!  Finally all the data files needed for AIPS are created.
$!  This procedure should be run with the default directory set to the
$!  directory corresponding to [AIPS] and the default disk set to
$!  the AIPS areas' default disk.  To run this procedure type
$!     $ SUBMIT IBATCH
$!  This procedure executes command file ASSIGNL.COM to activate
$!  necessary logical assignments.  Program FILAI2 reads file SYSPARM.
$!  Both of these files are created by the first main command procedure
$!  ILOAD.COM.
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
$!
$ deass FOR005
$                               ! Run file builder/init program that
$                               ! reads inputs from file SYSPARM.
$ SET protection=(S:RWED,O:RWED,G:RWE,W:RWE) /DEFAULT
$ run load:FILAI2
$                               ! Run POPSGN to initialize memory (ME) files.
$ RUN load:POPSGN
1 15 0 POPSDAT ' '
<CR>
