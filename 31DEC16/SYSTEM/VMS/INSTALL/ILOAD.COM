$!ILOAD.COM
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
$!  This is the first of the two main command procedure for installing
$!  AIPS.  This procedure will prompt the user for the AIPS system
$!  parameters, directory names, etc. needed to install AIPS.  Then the
$!  procedure creates the necessary subdirectories and loads the souce
$!  code from tape and into the correct subdirectories.  The user can
$!  then make any needed changes to the source code.  The user can
$!  compile the code and create the necessary AIPS data files by
$!  running the second main installation procedure BUILD.COM.
$!  This procedure should be run with the default directory
$!  set to the directory corresponding to [AIPS] and the default
$!  disk set to the AIPS default disk.  The tape drive should be
$!  mounted with the DCL command:
$!
$!  MOUNT /FOREIGN tape_drive_physical_name DUMMY TAPE:
$!
$!  then the initial command procedures should be copied with the DCL
$!  command:
$!
$!  BACKUP  TAPE:START.BCK []/NEW_VERSION
$!
$!  then type in @ILOAD and answer the questions, if you can.
$!---------------------------------------------------------------------
$                         ! Date of this release.
$ VERDATE == "15JUL91"
$                         ! Ask if we are in the correct directory.
$ @iCheckDef
$ if (RESULT.eqs."EXIT") then EXIT
$                         ! Prompt for AIPS parameters.
$ @iPrompt
$                         ! Write file [AIPS]SYSPARM to be used by
$                         ! program FILAI2.
$ @iSysParm
$                         ! Create permanent directory structure.
$ @iCreDir
$                         ! Copy the rest of our stuff from tape.
$ @iReadTap
$                         ! Create custom DCL procedures needed by
$                         ! this version.
$ @iCreDCL
$                         ! Create custom Option files needed by this
$                         ! Version.
$ @iCreOPT
$!
$ FINI:
$ write sys$output "The next installation command procedure is IBUILD."
$ EXIT
