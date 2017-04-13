$! COPYSUB.COM
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
$!----------------------------------------------------------------------
$! Copy a list of files from CVAX to local machine.
$! Inputs:
$!   p1    STR    name of file containing list of copy operations.
$! Each line of file STR to contain three strings:
$! 1. Full, precise VMS-path to CVAX directory.
$! 2. name of desired text file in directory (1)
$! 3. desired name for the text file in local directory (with full path).
$! DCW, NRAO-CV, 17July87/18Sept/26Oct.
$!----------------------------------------------------------------------
$ @UPD_PROC:Config
$ ftemp = "''Updtempdir'TRANSACT.LOG"
$ ftemp = f$search(ftemp)
$ if ftemp.nes."" then goto transactok
$    Open/write    Transact        'Updtempdir'TRANSACT.LOG
$    Close         Transact
$ transactok:
$!----------------------------------------------------------------------
$! Create internal file list:
$ Delete/NoLog                  'Updfilelist'.IN;*,.OUT;*
$ Open/Read     Updcopyfile     'P1'
$ Open/Write    Out_List        'Updfilelist'.IN
$ Ifile = 0
$ Copysub1:
$       Read/End_of_File=Thatsall0 Updcopyfile Line
$       @UPD_PROC:Parse 'Line'
$       Ifile = Ifile + 1
$       Updsource = "''Updv1'''Updv2'"
$       Updtempfile = "''Updfilename'''F$Fao("!4ZL",Ifile)'.TMP"
$       Updtarget = "''Updv3'"
$       Write Out_List "''Updsource' ''Updtempfile' ''Updtarget'"
$       Goto Copysub1
$ Thatsall0:
$ Close Updcopyfile
$ Close Out_List
$!----------------------------------------------------------------------
$ Set NoOn
$ If (Updprotocol.Eqs."DECNET").Or.(Updprotocol.Eqs."FTP") -
   .Or.(Updprotocol.Eqs."EMAIL") Then Goto 'Updprotocol'
$  Write Sys$Error "Unrecognized Updprotocol: ''Updprotocol'"
$  Goto Errorexit
$!
$!----------------------------------------------------------------------
$DECNET:
$ Open/Read     In_List 'Updfilelist'.IN
$ Copyloop1:
$       Read/End_of_File=Thatsall1 In_List Line
$       @UPD_PROC:Parse 'Line'
$       tries = 0
$!      Try to copy a file using DECnet 20 times before giving up:
$       Retryloop:
$               tries = tries + 1
$               Line = "Copy ''Nraocvax'''Cvaxdev'''Updv1'"
$               Line = "''Line' ''Updtempdir'''Updv2'"
$!              Write sys$output "Execute: ''Line'"
$               Line
$               If ($status) Then Goto Goodcopy
$                       If (tries.GT.20) Then Goto Quit1
$                       Wait 00:00:10.0
$                       GoTo Retryloop
$               Goodcopy:
$               Goto Copyloop1
$ Thatsall1:
$    Close In_List
$    Goto Movefiles
$ Quit1:
$    Write Updlogfile "** FAILS doing ",line
$    Close In_List
$    Goto Errorexit
$!
$!----------------------------------------------------------------------
$FTP:
$ If (Ftt.Eqs."EXOS").Or.(Ftt.Eqs."CMU") -
   .Or.(Ftt.Eqs."WIN") Then Goto Ftp2
$    Write Sys$Error "Unrecognized Ftt in FTP: ''Ftt'"
$    Goto Errorexit
$Ftp2:
$ Open/Read     In_List 'Updfilelist'.IN
$ Scratch2      :==     'Updtempdir'UPDCOPYSUB.FTP
$ Open/Write    Ftpf    'Scratch2'
$ If (Ftt.Eqs."EXOS") .Or. (Ftt.Eqs."WIN" ) Then -
                                Write Ftpf "open ''Nraocvax'"
$ If (Ftt.Eqs."CMU") Then       Write Ftpf "Set Host ''Nraocvax'"
$ If (Ftt.Eqs."EXOS") .Or. (Ftt.Eqs."WIN" ) Then -
                                Write Ftpf "user aips"
$ If (Ftt.Eqs."CMU") Then       Write Ftpf "aips"
$ Localcwd = F$Directory()
$ If (Ftt.Eqs."EXOS") .Or. (Ftt.Eqs."WIN" ) Then -
                                Write Ftpf "lcd ''Updtempdir'"
$ If (Ftt.Eqs."CMU") Then       Write Ftpf "Set Local ''Updtempdir'"
$ Ftploop1:
$       Read/End_of_File=Thatsall2 In_List Line
$       @UPD_PROC:Parse 'Line'
$       If (Ftt.Eqs."EXOS") .Or. (Ftt.Eqs."WIN" ) Then -
                Write Ftpf "get ''Cvaxdev'''Updv1' ''Updv2'"
$       If (Ftt.Eqs."CMU") Then -
                Write Ftpf "get ''Cvaxdev'''Updv1' ''Updv2'"
$       Goto Ftploop1
$ Thatsall2:
$ If  (Ftt.Eqs."EXOS") .Or. (Ftt.Eqs."WIN" ) .Or. (Ftt.Eqs."CMU") Then -
                Write Ftpf "quit"
$ Close In_List
$ Close Ftpf
$ Assign/Use 'Scratch2' Sys$Input
$ Ftp
$ Delete 'Scratch2';*
$ Set Default 'Localcwd'
$ Goto Movefiles
$!
$!----------------------------------------------------------------------
$ EMAIL:
$ T = F$Search("''Updfilelist'.DONE")
$ If "''T'".Nes."" Then Goto Emaildone
$ Copy/Log      'Updfilelist'.IN        'Updfilelist'.DOIT
$ Doitloop:
$       T = F$Search("''Updfilelist'.DONE")
$       If "''T'".Nes."" Then Goto Emaildone
$       Write SYS$OUTPUT "UPDCOPYSUB waiting on DONE."
$       Wait 00:01:00
$       Goto Doitloop
$ Emaildone:
$ Goto Movefiles
$!
$!----------------------------------------------------------------------
$ Movefiles:
$! Move files from temporary directory to final destinations:
$ Open/Read     In_List         'Updfilelist'.IN
$ Open/Append   Transact        'Updtempdir'TRANSACT.LOG
$ Copyloop9:
$       Read/End_of_File=Thatsall9 In_List Line
$       @UPD_PROC:Parse 'Line'
$       Copy    'Updtempdir''Updv2'     'Updv3'
$       If (.not.$status) Then Goto Quit9
$       Delete/NoLog    'Updtempdir''Updv2';*
$       Purge/NoLog     'Updv3'
$       Write Transact F$Time() +             ": Copied ",Updv1
$       Write Transact "                       : To     ",Updv3
$       Write Updlogfile F$Time() +             ": Copied ",Updv1
$       Write Updlogfile "                       : To     ",Updv3
$       Goto Copyloop9
$ Thatsall9:
$    Close In_List
$    Close Transact
$    Delete/NoLog  'Updfilelist'.*;*
$    Exit
$ Quit9:
$    Write Updlogfile "** FAILS copying ",Updv2," to ",Updv3
$    Close In_List
$    Close Transact
$!----------------------------------------------------------------------
$ Errorexit:
$    Write Updlogfile "** ERROR ** Could not copy ", P1
$    Exit %X1000002C
