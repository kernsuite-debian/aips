$! GETEMAIL
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
$!
$! Contains Pittsburgh specific file names
$!
$! Procedure to Wait for Files to Come in from AIPSSERV,
$! Move them from mailbox to scratch, decode them into
$! scratch directory, and verify that all of them have
$! been received. Times out eventually if it doesn't
$! get the files.
$! Execute as:		$@UPD_PROC:GetEmail
$! Assumes that 'updfilelist'.IN contains the list of files to be copied.
$! DCW, NRAO-CV, 29Sep87/30Sep.
$!-------------------------------------------------------------------------
$ @UPD_PROC:Config
$!
$ Saveit = "''F$Environment("Default")'"
$ Set Default 		'Updtempdir'
$ On Control_Y Then Goto Punt
$!-------------------------------------------------------------------------
$ If F$Search("''Updfilelist'.OUT").Nes."" Then Goto Outok
$	Open/Write	Out_List	'Updfilelist'.OUT
$	Close		Out_List
$ Outok:
$ Delete/NoLog			SCRATCH.IN;*
$!-------------------------------------------------------------------------
$ Open		In_List		'Updfilelist'.IN
$ Nwaiting = 0
$ Loopcount:
$	Read/End_Of_File=Thatsallcount	In_List	Line
$ 	Nwaiting = Nwaiting + 1
$	Goto Loopcount
$ Thatsallcount:
$ Close	In_List
$ Oldwaiting = Nwaiting
$ Write SYS$OUTPUT "Nwaiting=''Nwaiting'"
$!-------------------------------------------------------------------------
$! Times are in seconds.
$ Waitcount = 0
$ Waitlimit = 180 * 60
$ Deltawait = 40
$ Goto Readmail
$!-------------------------------------------------------------------------
$ Waitloop:
$ Waitcount = Waitcount + Deltawait
$ Write Sys$Output "[Waitcount=''Waitcount'" + ", Waitlimit=''Waitlimit'" -
		+ ", Nwaiting=''Nwaiting']"
$ If Waitcount.Gt.Waitlimit Then Goto Punt
$ Wait 00:00:'Deltawait'
$!-------------------------------------------------------------------------
$ Readmail:
$ T = F$Search("''Updtempdir'''Updfilename'%%%%.TMP")
$ If "''T'".Nes."" Then -
	Rename/Nolog	'Updtempdir''Updfilename'%%%%.TMP *.OLD
$ Assign/Use	SCRATCH.LIS	SYS$OUTPUT
$! New experimental mail extractor just below: (26oct87)
$ Mail
Select			NEWMAIL
1
Extract			SCRATCH.IN
Delete
$ Delete/NoLog		SCRATCH.LIS;*
$ Open/Error=Countem	Scratch_In	SCRATCH.IN
$ Close 		Scratch_In
$ Waitcount = 0
$!-------------------------------------------------------------------------
$ Run USR$ROOT3:[AIPSCLNT]Masher
LI=L1 SCRATCH.IN
$ Purge/NoLog		'Updtempdir''Updfilename'%%%%.*
$!-------------------------------------------------------------------------
$! Check if any files have been received.
$ S = F$Search ("SCRATCH.IN")
$ T = F$Search("''Updtempdir'''Updfilename'%%%%.TMP")
$ If "''T'".Nes."" Then Goto Filereceived
$! Whoops, no file received:
$ 	Rename/Nolog	'S'	SCRATCH.BAD
$	B = F$Search ("SCRATCH.BAD")
$ 	Write Sys$Output "Rename ''S' ''B'"
$	Goto Punt7
$ Filereceived:
$ 	Delete/NoLog	'S'
$	Write	SYS$OUTPUT 	"Received ''T'"
$ Punt7:
$ Goto Readmail
$!-------------------------------------------------------------------------
$! Check if any files have been received. If so, move their list items from
$! 	'Updfilelist'.IN to 'Updfilelist'.OUT.
$ Countem:
$ T = F$Search("''Updtempdir'''Updfilename'%%%%.OLD")
$ If "''T'".Nes."" Then -
	Rename/Nolog	'Updtempdir''Updfilename'%%%%.OLD *.TMP
$ Open		In_List		'Updfilelist'.IN
$ Open/Append	Out_List	'Updfilelist'.OUT
$ Open/Write	Tmp_List	'Updfilelist'.IOT
$ Nwaiting = 0
$ Loop2:
$	Read/End_Of_File=Thatsall2	In_List	Line
$	T = "''Updtempdir'''F$Element(1," ", Line)'"
$	S = F$Search("''T'")
$	If "''S'".Eqs."" Then Goto Notyet2
$		Write Out_List "''Line'"
$		Goto Loop2
$	Notyet2:
$		Write Tmp_List "''Line'"
$		Nwaiting = Nwaiting + 1
$		Goto Loop2
$ Thatsall2:
$ Close	In_List
$ Close Out_List
$ Close Tmp_List
$ Rename	'Updfilelist'.IOT	.IN
$ Purge/NoLog	'Updfilelist'.IN,.OUT
$ Write SYS$OUTPUT "Nwaiting=''Nwaiting'"
$!-------------------------------------------------------------------------
$ If Nwaiting.Gt.0 Then Goto Waitloop
$! No more files waiting; they are all here now. Let's move them!
$ Rename/Nolog	'Updtempdir''Updfilename'%%%%.OLD	*.TMP
$ Rename/NoLog	'Updfilelist'.OUT	.IN
$ Purge/NoLog	'Updfilelist'.IN
$ Goto Quit
$!-------------------------------------------------------------------------
$! The waitloop timed out!
$ Punt:
$ Write Sys$Output "Unable to get all requested files. Aborting."
$ Set Default		'Saveit'
$ Exit 444
$!-------------------------------------------------------------------------
$ Quit:
$ Set Default		'Saveit'
