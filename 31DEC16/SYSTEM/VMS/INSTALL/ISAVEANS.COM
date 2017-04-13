$!iSaveAns.COM
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
$!  This routine saves all AIPS parameters set by the user in IPROMPT
$!  Thus, old answers can be reloaded instead of reinput.
$!  INPUTS:
$!    PARM'I'      STR   The system parameter names.
$!    
$!  OUTPUTS:
$!    LastTime.COM FILE  A text file containing the system parameters.
$!                       This file is found in area AIPS_ROOT.
$!---------------------------------------------------------------------
$!
$ write sys$output " "
$ write sys$output "Saving your answers"
$                                 ! Create and write system parameters
$ open /write Parms iLastTime.Com
$ write Parms -
    "$! This File Contains all entries from the last run of IPROMPT."
$                                    ! Save Version
$ write Parms "$ THISVER :== ",THISVER
$                                    ! First group of parms.
$ I = 0
$ WRITE:
$   I = I + 1
$   ThisParm = PARM'I'
$   Number = 'ThisParm'
$   if (PTYPE'I'.eqs."S").or.(PTYPE'I'.eqs."F") then -
      write Parms "$ ", ThisParm, " :== ",'ThisParm'
$   if (PTYPE'I'.eqs."I") then write Parms "$ ", ThisParm, " == ",'ThisParm'
$ if (I .lt. TOTQUEST) then GOTO WRITE
$                                    ! Second group of parms.
$ I = 100
$ WRITE:
$   I = I + 1
$   ThisParm = PARM'I'
$   Number = 'ThisParm'
$   if (PTYPE'I'.eqs."S").or.(PTYPE'I'.eqs."F") then -
      write Parms "$ ", ThisParm, " :== ",'ThisParm'
$   if (PTYPE'I'.eqs."I") then write Parms "$ ", ThisParm, " == ",'ThisParm'
$ if (I .lt. NumQUEST) then GOTO WRITE
$ close Parms
$ EXIT
