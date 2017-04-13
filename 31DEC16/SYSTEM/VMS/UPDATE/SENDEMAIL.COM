$!----------------------------------------------------------------------
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
$! Procedure to compose an E-mail request message to AIPSSERV@NRAO.
$! Execute as:          $@UPD_PROC:SendEmail
$! Assumes that "''Updfilelist'.IN" contains the list of
$!      files to be copied.
$! DCW, NRAO-CV, 21Sept87/30Sep.
$!----------------------------------------------------------------------
$ @UPD_PROC:Config
$!
$ Open/Read     In_List         'Updfilelist'.IN
$ Scratch2      :==             'Updfilelist'.REQ
$ Open/Write    Emailreq        'Scratch2'
$ Emailloop1:
$       Read/End_of_File=Thatsall3 In_List Line
$       @UPD_PROC:Parse   'Line'
$       Updsource       = "''Updv1'"
$       Updtempfile     = "''Updv2'"
$       Write Emailreq  "SEND ''Updsource' ''Updtempfile'"
$       Goto Emailloop1
$ Thatsall3:
$ Close In_List
$ Close Emailreq
$! Construct Address of AIPSSERV:
$ Scratch3 :== "''Updtempdir'SCRATCH.DIS"
$ Open/Write    Scratch_Dis     'Scratch3'
$ If Ftt.Eqs."EXOS" Then Write Scratch_Dis "EXOS%""''Nraocvax'""
$ If Ftt.Eqs."JNET" Then Write Scratch_Dis "JNET%""''Nraocvax'""
$ If Ftt.Eqs."ANL"  Then Write Scratch_Dis "GATEWAY::""''Nraocvax'""
$ Close         Scratch_Dis
$ Mail/Subject="AIPS-Update-File-Request" 'Scratch2' "@''Scratch3'"
$ Delete/NoLog  'Scratch3';*,'Scratch2';*
