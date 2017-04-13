$! ASSNLOCAL - Set up local AIPS logical names
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
$! The logical names may be defined in various tables depending upon
$! how this procedure is invoked. P1 is a suitable DEFINE command and
$! any DEFINE's must be derived from this.
$!
$! P1 = a suitable DEFINE command to be used
$!
$! Two flavours of the DEFINE command
$!
$       DEF := 'P1'/NOLOG
$       DEC := 'P1'/NOLOG/TRANSLATION_ATRIBUTES=CONCEALED
$!
$! AIPS Versions - up to three allowed (OLD, NEW, TST)
$! Note that physical device names must be used here in order that
$! the rooted logical names work correctly.
$! WARNING! Procedure ASSNSTART also contains a hardcoded version
$! date which must be changed when the following lines are changed.
$!
$       DEC OLD       DUB0:[AIPS.15JUL90.]
$       DEC NEW       UMA0:[AIPS.15OCT90.]
$       DEC TST       UMA0:[AIPS.15JAN91.]
$       DEC LOCAL     UMA0:[AIPS.LOCAL.]
$       DEC AIPS_ROOT UMA0:[AIPS.]
$       DEC DBG       DUB0:[AIPS.AIPS.]
$!
$! Override default version at NRAO
$!
$       DEF AIPS_VERSION TST
$!
$! Data areas
$!
$       DEF DA00 UMA1:[AIPS.DATA]
$       DEF DA01 UMA1:[AIPS.TDATA]
$       DEF DA02 DISK$RES:[AIPS.TDATA]
$       DEF DA03 UMA2:[AIPS.TDATA]
$!
$! FITS data area (known in ASSNBASIC as AIPS_ROOT:[FITS])
$!
$       DEF FITS UMA1:[AIPS.FITS]
$!
$! LOCAL areas
$!
$       DEF LOCRUN LOCAL:[RUN]
$       DEF LOCLOD LOCAL:[LOAD]
$       DEF LOCSYS LOCAL:[SYSTEM.VMS]
$!
$! Magnetic tapes
$!
$       DEF MT00 __MTA0:
$       DEF MT01 __MMA0:
$!
$! Reserved AIPS terminals
$!
$       DEF RESSTT1 __TTA5:
$!
$! Message terminals
$!
$       DEF TASKTT1 __TTB0:
$!
$! Batch log terminal
$!
$       DEF BATCH_OUT __TTB0:
$!
$! TEK terminals
$!
$       DEF TEKTK1 __TTA4:
$!
$! TV
$!
$       DEF TVDEV1 __IIA0:
$!      DEF TVDEV2 __IIB0:
$!
$! Plotters
$!
$       DEF VERPLOT SYS$PRINT           ! versatek plot/print
$       DEF LASQUE QMS                  ! laser plot/print queue
$       DEF LPRQUE SYS$PRINT            ! line printer queue
$       DEF PLOTTER FITS                ! temp area for plot file
$       DEF LPRNTR PRTFIL               ! temp area for print file
