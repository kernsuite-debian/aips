$!ISYSPARM.COM
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
$!  This routine writes AIPS parameters needed by FORTRAN program
$!  FILAI2 to a text file [AIPS.new.DOC.TEXT]SYSPARM.
$!  INPUTS:
$!    PARM'I'    INT   The system parameters.
$!    QUEST'I'   STR   A description of the system parameters.
$!    NO_QUEST   INT   The number of system parameters (start with 1).
$!    NEWDIR     STR   The subdirectory corresponding to [AIPS.new
$!  OUTPUTS:
$!    SYSPARM.   FILE  A text file containing the system parameters.
$!                     This file is found in area AIPS_ROOT.
$!---------------------------------------------------------------------
$!
$ write sys$output "Creating installation parameter file"
$                                 ! Create and write system parameters
$                                 ! into temporary file to be read
$                                 ! by program FILAI2.
$ open /write Parms SYSPARM.
$                                 ! First line is version date.
$ write Parms -
    f$extract(0,2,VERDATE),"-",f$extract(2,3,VERDATE),"-", -
    "19",f$extract(5,2,VERDATE)
$!
$ I = 0
$ WRITE:
$   I = I + 1
$   ThisParm = PARM'I'
$   Number = 'ThisParm'
$   if (PTYPE'I'.eqs."S") then GOTO CONT
$   if (PTYPE'I'.eqs."I") then GOTO INTEGER
$                                 ! Change answer to floating
$                                 ! point if necessary.
$      Length = f$length(Number)
$      Position = f$locate(".",Number)
$      if (Position .EQ. Length) then Number := 'Number'.0
$   GOTO CONT
$   INTEGER:
$      Number := 'Number'.0
$!
$   CONT:
$                                ! Don't print comment for system name
$                                ! or APDIR at all.
$   if (ThisParm.nes."SYSNAME").and.(ThisParm.nes."APDIR") then -
       write Parms Number, "       ! ", I, "   ", QUEST'I'
$   if (ThisParm.eqs."SYSNAME") then write Parms Number
$ if (I .lt. TOTQUEST) then GOTO WRITE
$ close Parms
$!      
$ EXIT
