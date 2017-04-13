$! RUNAIPS.COM - tell users how to start up AIPS.
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
$  CHOICE = ""
$  IF (F$TRNLNM("OLD").NES."") THEN CHOICE = "AIPS OLD"
$  IF (F$TRNLNM("NEW").EQS."") THEN GOTO TRYTST
$     IF (CHOICE.NES."") THEN CHOICE = CHOICE + " or AIPS NEW"
$     IF (CHOICE.EQS."") THEN CHOICE = "AIPS NEW"
$  TRYTST:
$  IF (F$TRNLNM("TST").EQS."") THEN GOTO DOIT
$     IF (CHOICE.NES."") THEN CHOICE = CHOICE + " or AIPS TST"
$     IF (CHOICE.EQS."") THEN CHOICE = "AIPS"
$  DOIT:
$  IF (CHOICE.EQS."AIPS NEW") THEN CHOICE = "AIPS"
$  IF (CHOICE.EQS."") THEN WRITE SYS$OUTPUT -
      "**ERROR** : No logical symbols defined for running AIPS"
$  IF (CHOICE.NES."") THEN WRITE SYS$OUTPUT -
      "To start up AIPS type ",CHOICE
