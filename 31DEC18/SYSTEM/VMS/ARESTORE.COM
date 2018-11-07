$! ARESTORE.COM
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
$! Restore AIPS save sets backed up with ABACKUP.
$!----------------------------------------------------------------------
$ WSO = "WRITE SYS$OUTPUT"
$ 'WSO' " "
$ 'WSO' "Beware of possible file format changes since your ABACKUP."
$ 'WSO' " "
$ QTAPE:
$   INQUIRE TN "Enter the AIPS tape drive number"
$   TN = TN - 1
$   MT = "MT0" + F$STRING(TN)
$   MT = f$trnlnm(MT)
$   IF (MT.NES."") THEN GOTO XTAPE
$     'WSO' "I can not find tape drive MT0",TN
$     GOTO QTAPE
$ XTAPE:
$!
$ MOUNTED = F$GETDVI(MT,"MNT")
$ IF (MOUNTED.EQS."FALSE") THEN MOUNT /FOR 'MT'
$!
$ 'WSO' " "
$ QINDEX:
$   INQUIRE ANS "Do you want a (slow) index of data sets on tape? (Y or N)"
$   IF (ANS.EQS."N") THEN GOTO XINDEX
$ IF (ANS.NES."Y") THEN GOTO QINDEX
$                                       ! Index requires mounted non foreign.
$   DISMOUNT /NOUNLOAD 'MT'
$   MOUNT /OVER=ID  'MT'
$   DIR /DATE/SIZE  'MT'
$   DISMOUNT /NOUNLOAD 'MT'
$   MOUNT /FOR 'MT'
$   'WSO' " "
$ XINDEX:
$!
$ 'WSO' " "
$ QSKIP:
$ ON ERROR THEN GOTO QSKIP
$   'WSO' "Enter the number of data sets to skip (If you do not have save"
$   'WSO' "sets on tape with duplicate names then you can enter zero)"
$ INQUIRE SKIP "Enter"
$!
$ COUNT = 0
$ BSKIP:
$   COUNT = COUNT + 1
$   IF (COUNT.GT.SKIP) THEN GOTO XSKIP
$   BACKUP /LIST=NLA0: 'MT'
$ GOTO BSKIP
$ XSKIP:
$!
$ 'WSO' " "
$ QUSER:
$   ON ERROR THEN GOTO QUSER
$   INQUIRE UN "Enter user number"
$   HUN  = F$INTEGER(UN)
$   HUN = F$FAO("!3XL",HUN)
$!
$ 'WSO' " "
$ QDISK:
$   ON ERROR THEN GOTO QDISK
$   INQUIRE DN "Enter AIPS disk number (zero implies all disks)"
$   ISDISK = 1
$   IEDISK = 9
$   IF (DN.EQ.0) THEN GOTO XDISK
$     ISDISK = DN
$     IEDISK = DN
$ XDISK:
$                                       ! Verify no catalogs on disks.
$ DD1 = ISDISK
$ SET ON
$ VLOOP:
$   DISK = "DA0" + F$STRING(DD1)
$   DL = f$trnlnm(DISK)
$   IF (DL.EQS."") THEN GOTO RESTORE
$   OPEN /READ/ERROR=CATOK TEMP 'DISK':CA'DD1'00000.'HUN';1
$     CLOSE TEMP
$     'WSO' " "
$     'WSO' "* Fatal condition *"
$     'WSO' "You have a catalog on disk ",DD1, ".  This program is not"
$     'WSO' "sophisticated enough to put the data sets on tape into"
$     'WSO' "your catalog on disk.  You must clear all of your data"
$     'WSO' "from this disk."
$      INQUIRE ANS "Press RETURN to exit"
$      GOTO FINI
$   CATOK:
$   DD1 = DD1 + 1
$ IF (DD1.LE.IEDISK) THEN GOTO VLOOP
$!
$ RESTORE:
$ DD1 = ISDISK
$ SET NOON
$ 'WSO' " "
$ RLOOP:
$   DISK = "DA0" + F$STRING(DD1)
$   DL = f$trnlnm(DISK)
$   IF (DL.EQS."") THEN GOTO NEXT
$   'WSO' "Restoring AIPS disk ", DD1
$   BACKUP /NOREWIND  /VERIFY 'MT'DSK'DD1'V1.'HUN' 'DISK':*.'HUN'
$   DD1 = DD1 + 1
$ IF (DD1.LE.IEDISK) THEN GOTO RLOOP
$ 'WSO' " "
$ NEXT:
$   INQUIRE ANS "Do you want to restore another disk from the same tape (Y or N)"
$   IF (ANS.EQS."Y") THEN GOTO QDISK
$   IF (ANS.NES."N") THEN GOTO NEXT
$ FINI:
$ DISMOUNT 'MT'
$ EXIT
