$! IQUEST.COM
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
$! This routine can be used to display, prompt, and check the range
$! for a series of values set up by the calling program.  Communication
$! is through global variables Parm'I', QUEST'I", LOWERLIM'I', and
$! UPPERLIM'I', where 'I' is an integer range of values determined
$! by input parameters.
$! PARMS:
$!   P1    The first value of the form Parm'P1', Quest'P1', etc.
$!   P2    The last value of the form Parm'P1', Quest'P1', etc.
$!---------------------------------------------------------------------
$ P1 = f$integer(P1)
$ P2 = f$integer(P2)
$ PRINT:
$   I = P1
$   K = 1
$   write sys$output -
      "NO.  PARAMETER                                       VALUE"
$   LOOP:
$     T = Parm'I'
$     if (K.lt.10) then write sys$output " ",K,"  ", Quest'I', "   ", 'T'
$     if (K.ge.10) then write sys$output     K,"  ", Quest'I', "   ", 'T'
$     I = I + 1
$     K = K + 1
$   if I.le.P2 then GOTO LOOP
$!
$ SELECT:
$   on WARNING then GOTO SELECT
$   inquire Option -
      ">> ENTER NUMBER TO CHANGE, 0=PRINT, -1=FINISHED, -2=PREVIOUS MENU"
$   if (Option.eq.-1) .or. (Option.eq.-2) then GOTO FINI
$   if Option.eq.0  then GOTO PRINT
$   Option = f$integer(Option) + P1 - 1
$   Option = f$string(Option)
$   if (Option .lt. P1) .or. (Option .GT. P2) THEN GOTO SELECT
$                                 ! Ask for single value.
$   ASK:
$     on warning then GOTO ASK
$     write sys$output Quest'Option'
$     inquire Tempo "ENTER"
$     if (PTYPE'Option'.eqs."S").or.(PTYPE'Option'.eqs."F") then GOTO OK
$     if (Tempo.ge.LowerLim'Option') .and. -
         (Tempo.le.UpperLim'Option') then GOTO OK
$!
$        write Sys$Output " *RANGE ERROR* ", -
           "ACCEPTIBLE RANGE: ",LowerLim'Option', -
           " TO ",UPPERLIM'OPTION'
$        GOTO ASK
$!
$        OK:
$        T = Parm'Option'
$        'T' :== 'Tempo'
$        GOTO SELECT
$!
$ FINI:
$ QUESTRETURN == Option
$ EXIT
