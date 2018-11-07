$! ASSNBASIC - Set up fundamental AIPS logical names
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
$! Two flavors of the DEFINE command
$!
$       DEF := 'P1'/NOLOG
$       DEC := 'P1'/NOLOG/TRANSLATION_ATRIBUTES=CONCEALED
$!
$! Default version
$!
$       DEF AIPS_VERSION NEW
$!
$! AIPS procedures.
$!
$       DEF AIPS_PROC SYSLOCAL,SYSVMS,SYSAIPS
$       DEF SYSLOCAL  AIPS_VERSION:[SYSTEM.VMS.LOCAL]
$       DEF SYSVMS    AIPS_VERSION:[SYSTEM.VMS]
$       DEF SYSAIPS   AIPS_VERSION:[SYSTEM]
$!
$! RUN and FITS disk areas
$!
$       DEF RUNFIL      AIPS_ROOT:[RUN]
$       DEF OFMFIL      AIPS_ROOT:[OFM]
$       DEF RUNSYS      AIPS_VERSION:[RUN]
$       DEF FITS        AIPS_ROOT:[FITS]
$       DEF PRTFIL      AIPS_ROOT:[PRINT]
$!
$! TEXT areas
$!
$       DEF AIPSIONS    AIPS_ROOT:[TEXT.IONS]
$       DEF AIPSPUBL    AIPS_ROOT:[TEXT.PUBL]
$       DEF AIPSOFM     AIPS_ROOT:[TEXT.OFM]
$       DEF AIPSGRIP    AIPS_ROOT:[TEXT.GRIP]
$       DEF AIPSWHO     AIPS_ROOT:[TEXT.WHO]
$!
$! Important areas for TST
$!
$       DEF TSTMEM      TST:[MEMORY]
$       DEF TSTPSAP     TST:[LOAD.ALT1]
$!
$! Important areas for NEW
$!
$       DEF NEWMEM      NEW:[MEMORY]
$       DEF NEWPSAP     NEW:[LOAD.ALT1]
$!
$! Important areas for OLD
$!
$       DEF OLDMEM      OLD:[MEMORY]
$       DEF OLDPSAP     OLD:[LOAD.ALT1]
$!
$! Default terminal channels
$! TASK_IN and OUT must not be assigned to the process permanent SYS$INPUT
$! and SYS$OUTPUT.
$!
$       DEF FOR005      SYS$INPUT
$       DEF FOR006      SYS$OUTPUT
$       DEF TASK_IN     TT
$       DEF TASK_OUT    TT
