$!CONINC.COM
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
$! Use: @CONINC directory (name)
$! This procedure concatenates the D*.INC C*.INC and E*.INC files
$! in directory name "directory".
$!  P1 = Directory (default = current)
$!  P2 = Name of file to process (optional, default=all *.FOR, *.INC)
$!----------------------------------------------------------------------
$!                                       Save current directory
$CUR_DIR = F$DIRECTORY()
$!                                       Change default
$IF (P1.NES."") THEN SET DEFAULT 'P1'
$!                                       Optional list
$OPTION := "D*.INC"
$IF (P2.NES."") THEN OPTION := 'P2'
$WRITE SYS$OUTPUT "Concatenating ",OPTION," files in ",F$DIRECTORY()
$ON ERROR THEN GOTO FINISH
$!                                       Get directory listing
$DIR/NOTRAIL/VER=1/COL=1/OUTPUT=DIRTEMP.LIS   'OPTION'
$!                                       Open input
$OPEN /READ IN DIRTEMP.LIS
$!                                       Skip first 3 lines
$   READ/END_OF_FILE=DONE IN RECORD
$   READ/END_OF_FILE=DONE IN RECORD
$   READ/END_OF_FILE=DONE IN RECORD
$!                                       Create output file
$OPEN /WRITE OUT CONINC.LIS
$LOOP:
$        READ/END_OF_FILE=DONE IN RECORD
$        WRITE OUT RECORD
$        GOTO LOOP
$DONE:
$CLOSE IN
$CLOSE OUT
$DELETE DIRTEMP.LIS;*
$!                                       Convert files
$RUN LOAD:CONINC
$FINISH:
$SET DEFAULT 'CUR_DIR'
