$! UPDSRTUNQ.COM
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
$!-----------------------------------------------------------------------
$! Copy all CVAX control files containing lists of modified files,
$! sort them and get rid of duplicates (except for REASONS).
$! Signals:
$!   $status
$!-----------------------------------------------------------------------
$                                ! Sort Them
$ sort /key=(position:35,size:22)/key=(position:22,size:8,descending) -
    /key=(position:13,size:8,descending) 'P1'.upd  'P1'.srt
$ Open/Read srtfile 'P1'.srt
$ Open/Write unqfile 'P1'.unq
$ Open/Read lasttime lastgood.dat
$                                ! Get last date in proper format.
$ Read lasttime line
$ Close lasttime
$ lastdate = f$cvtime(line,"COMPARISON","DATE") - "-" - "-"
$ !
$ set noon
$ lastname = ""
$ LOOP:
$   On WARNING Then GOTO LOOP
$   Read/END_OF_FILE=END_LOOP/ERROR=END_LOOP srtfile line
$   CmprsLine = f$edit(line,"COMPRESS,TRIM")
$   if (line.EQS."") then GOTO LOOP
$   date = f$element(2," ",CmprsLine)
$   If (date .LTS. lastdate) Then GOTO LOOP  ! We already did this one.
$     subdir = f$element(4," ",CmprsLine)
$     thisname = subdir + ":" + f$element(5," ",CmprsLine)
$     if ((subdir.eqs."APLMC4") .or. -
          (subdir.eqs."APLCOS") .or. -
          (subdir.eqs."APLPSC") .or. -
          (subdir.eqs."APLUNIX") .or. -
          (subdir.eqs."APLBELL") .or. -
          (subdir.eqs."APLSYS3") .or. -
          (subdir.eqs."APLMASC") .or. -
          (subdir.eqs."APLSYS5") .or. -
          (subdir.eqs."APLV7") .or. -
          (subdir.eqs."APLUTS")) then GOTO LOOP
$     IF ((subdir.eqs."APLBERK") .or. -
          (subdir.eqs."APL4PT1") .or. -
          (subdir.eqs."APL1VAX") .or. -
          (subdir.eqs."APL4PT2") .or. -
          (subdir.eqs."APLCVEX") .or. -
          (subdir.eqs."APLALLN") .or. -
          (subdir.eqs."APL2VAX") .or. -
          (subdir.eqs."APLSUN")) then GOTO LOOP
$     if ((subdir.eqs."APLNUNIX") .or. -
          (subdir.eqs."APLNMC4") .or. -
          (subdir.eqs."APLNCOS") .or. -
          (subdir.eqs."APGUNIX") .or. -
          (subdir.eqs."APGMC4") .or. -
          (subdir.eqs."APGCOS")) then GOTO LOOP
$     if ((subdir.eqs."Q5000") .or. -
          (subdir.eqs."QFPS32") .or. -
          (subdir.eqs."Q190") .or. -
          (subdir.eqs."QCOS") .or. -
          (subdir.eqs."QVEX") .or. -
          (subdir.eqs."QALN")) then GOTO LOOP
$     if ((subdir.eqs."QPGUNIX") .or. -
          (subdir.eqs."QPGMC4") .or. -
          (subdir.eqs."QPGCOS") .or. -
          (subdir.eqs."YPGUNIX") .or. -
          (subdir.eqs."YPGMC4") .or. -
          (subdir.eqs."YPGCOS") .or. -
          (subdir.eqs."QYPGUNIX") .or. -
          (subdir.eqs."QYPGMC4") .or. -
          (subdir.eqs."QYPGCOS")) then GOTO LOOP
$     if ((subdir.eqs."AIPGUNIX") .or. -
          (subdir.eqs."AIPGMC4") .or. -
          (subdir.eqs."AIPGCOS") .or. -
          (subdir.eqs."INCUNIX") .or. -
          (subdir.eqs."INCMC4") .or. -
          (subdir.eqs."INCCOS") .or. -
          (subdir.eqs."INCVEX") .or. -
          (subdir.eqs."INCALN")) then GOTO LOOP
$     if ((subdir.eqs."SYSUNIX") .or. -
          (subdir.eqs."INSUNIX") .or. -
          (subdir.eqs."SYSSUN") .or. -
          (subdir.eqs."SYSCVEX") .or. -
          (subdir.eqs."SYSALLN") .or. -
          (subdir.eqs."SYSMC4") .or. -
          (subdir.eqs."SYSCOS") .or. -
          (subdir.eqs."INSCOS") .or. -
          (subdir.eqs."SYSPSC") .or. -
          (subdir.eqs."SYSLOCAL")) then GOTO LOOP
$     If ((subdir.eqs."DOCGRIP") .or. -
          (subdir.eqs."DOCWHO")) then GOTO LOOP
$     if ((thisname.eqs.lastname) .and. -
          (P1.nes."REASONS")) then GOTO LOOP
$     Write unqfile line
$     lastname = thisname
$     GOTO LOOP
$ END_LOOP:
$ close srtfile
$ close unqfile
$ EXIT
