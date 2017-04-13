$!TRANSPRT.COM
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
$! Produce transport tape.
$!---------------------------------------------------------------------
$INQUIRE MT "Enter tape drive name (with :)"
$INQUIRE AREA "Enter area to transport from (like 15NOV82)"
$INQUIRE DENS "Enter density (1600 or 6250)"
$DUMMY = F$EXTRACT(2,5,AREA)
$IF 'DENS' .EQS. "1600" THEN INITIALIZE /DENS=1600 'MT' 'DUMMY'
$IF 'DENS' .EQS. "6250" THEN INITIALIZE /DENS=6250 'MT' 'DUMMY'
$MOUNT/FOREIGN 'MT' DUMMY TAPE:
$ WRITE SYS$OUTPUT "Top level procedures"
$BACKUP  UMA0:[AIPS.'AREA'.SYSTEM.VMS.INSTALL]*.*;  TAPE:START.BCK
$ IF 'DENS'.EQS."6250" THEN GOTO HIGHD
$!                                       restricted writes
$ SET ON
$ WRITE SYS$OUTPUT "Sending ALMOST ALL OF AIPS AT 1600"
$BACKUP /NOREW/EXCLUDE=  (                              -
               [AIPS.'AREA'.MEMORY]*.*,                 -
               [AIPS.'AREA'.ERRORS]*.*,                 -
               [AIPS.'AREA'.UPDATE]*.*,                 -
               [AIPS.'AREA'.LOAD.TEMP]*.*,              -
               [AIPS.'AREA'.LOAD]*.*,                   -
               [AIPS.'AREA'.LOAD.ALT1]*.*,              -
               [AIPS.'AREA'.LOAD.ALT2]*.*,              -
               [AIPS.'AREA'.LOAD.ALT3]*.*,              -
               [AIPS.'AREA'.SYSTEM.VMS]*2.OPT,          -
               [AIPS.'AREA'.SYSTEM.VMS]*3.OPT,          -
               [AIPS.'AREA'.SYSTEM.VMS.LOCAL]*.*,       -
               [AIPS.'AREA'.DOC.TEXT]MV2C10*.*,         -
               [AIPS.'AREA'.DOC.TEXT]MV2C11*.*,         -
               [aips.text.grip]*.*,                     -
               [aips.text.who]*.*,                      -
               [AIPS.text.PUBL]LET8*.TEX,               -
               [AIPS.text.PUBL]COOKUP.TEX,              -
               [AIPS.text.PUBL]COOKZOS.TEX,             -
               [AIPS.text.PUBL]UPC*.TEX,                -
               [AIPS.'AREA'.HIST]CHANGED.*,             -
               [AIPS.'AREA'.HIST]HISTRY.*,              -
               [AIPS.'AREA'.HIST]*.CHK                  -
                        )                               -
       uma0:[aips.text...]*.*;*,UMA0:[AIPS.'AREA'...]*.*; -
       TAPE:AIPS.BCK
$ GOTO ENDIT
$HIGHD:
$ WRITE SYS$OUTPUT "Sending ALL OF AIPS AT 6250"
$BACKUP /NOREW/EXCLUDE=  (                              -
               [AIPS.'AREA'.MEMORY]*.*,                 -
               [AIPS.'AREA'.ERRORS]*.*,                 -
               [AIPS.'AREA'.UPDATE]*.*,                 -
               [AIPS.'AREA'.LOAD.ALT2]*.*,              -
               [AIPS.'AREA'.LOAD.ALT3]*.*,              -
               [AIPS.'AREA'.SYSTEM.VMS]*2.OPT,          -
               [AIPS.'AREA'.SYSTEM.VMS]*3.OPT,          -
               [AIPS.'AREA'.SYSTEM.VMS.LOCAL]*.*,       -
               [aips.text.grip]*.*,                     -
               [aips.text.who]*.*                       -
                        )                                -
       uma0:[aips.text...]*.*;*,UMA0:[AIPS.'AREA'...]*.*; -
       TAPE:AIPS.BCK
$!                                           dismount the tape(s)
$ ENDIT:
$ DISMOUNT TAPE:
