$! ABACKUP.COM
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
$!------------------------------------------------------------------------
$! This routine will backup the data for a specific user in a specific
$! AIPS data area.
$!------------------------------------------------------------------------
$ SET ON
$ WSO = "WRITE SYS$OUTPUT"
$ 'WSO' " "
$ 'WSO' " "
$ 'WSO' "Warning! These limitations apply:"
$ 'WSO' "   a. This tape can be restored ONLY on VAX/VMS systems."
$ 'WSO' "   b. You must backup a complete disk"
$ 'WSO' "   c. When you restore to a disk you must restore to a disk"
$ 'WSO' "      with the SAME AIPS DISK NUMBER, and restore to the"
$ 'WSO' "      SAME USER NUMBER as the data on tape."
$ 'WSO' "   d. The backup can not merge a users data on disk with"
$ 'WSO' "      the data restored from tape. Therefore, when you do the"
$ 'WSO' "      restore, the disk must not have ANY data on it with"
$ 'WSO' "      the same user number as the data on tape."
$ 'WSO' "   e. The AIPS version must be 15MAY84 or later and the AIPS"
$ 'WSO' "      must have been installed with the private catalog option."
$ 'WSO' "   f. The reorganization of AIPS data into TST and NEW/OLD has"
$ 'WSO' "      made the backup of save/get and message file more"
$ 'WSO' "      complicated.  This program can not backup these files at"
$ 'WSO' "      this time."
$ 'WSO' " "
$!
$ QTAPE:
$   INQUIRE TN "Enter the AIPS tape drive number"
$   TN = TN - 1
$   MT = "MT0" + F$STRING(TN)
$   MT = F$TRNLNM(MT)
$   IF (MT.NES."") THEN GOTO XTAPE
$     'WSO' "I can not find tape drive MT0",TN
$     GOTO QTAPE
$ XTAPE:
$!
$ 'WSO' " "
$ QDENS:
$   INQUIRE DENS "Enter tape density (800, 1600, 6250) "
$   IF (DENS.NES."800") .AND. (DENS.NES."1600") .AND. (DENS.NES."6250") -
      THEN  GOTO QDENS
$!
$ MOUNTED = F$GETDVI(MT,"MNT")
$ IF (MOUNTED.EQS."FALSE") THEN MOUNT /FOR /DENS='DENS' 'MT'
$!
$ 'WSO' " "
$ QUSER:
$   ON ERROR THEN GOTO QUSER
$   INQUIRE UN "Enter user number"
$   HUN  = F$INTEGER(UN)
$   HUN = F$FAO("!3XL",HUN)
$   'WSO' " "
$   'WSO' "Your hexadecimal user number is ",HUN,"."
$   'WSO' "I will backup all files of the form *.",HUN
$   'WSO' " "
$!
$ QSTRT:
$   ON ERROR THEN GOTO QSTRT
$   'WSO' "1=Start at the beginning of the tape (Clobber old data on tape).
$   'WSO' "2=Go to the end of the tape. (This may produce save sets with the"
$   'WSO' "same name and require considerable user expertise on the restore)."
$   INQUIRE STRT "Enter "
$   REWIND = " "
$   IF (STRT.EQS."1") THEN REWIND = "/REWIND"
$   IF (STRT.EQS."2") THEN REWIND = "/NOREWIND"
$   IF (REWIND.EQS." ") THEN GOTO QSTRT
$!
$ 'WSO' " "
$ QLOG:
$   ON ERROR THEN GOTO QLOG
$   LOG = " "
$   INQUIRE LN "Should I type each files's name when I back it up? (Y or N)"
$   IF (LN.EQS."Y") THEN LOG = "/LOG"
$   IF (LN.EQS."N") THEN LOG = "/NOLOG"
$   IF (LOG.EQS." ") THEN GOTO QLOG
$!
$ AGAIN:  ! Starts over here if user wants another disk backed up.
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
$!
$ 'WSO' " "
$ QDEL:
$   IF (DN.NE.0) THEN -
      'WSO' "Should I delete files after I back them up for user ",-
        UN, " on disk ", DN
$   IF (DN.EQ.0) THEN -
      'WSO' "Should I delete files after I back them up for user ", -
        UN, " on all disks"
$   INQUIRE DYN "Enter Y or N"
$   DELFIL = "X"
$   IF (DYN.EQS."Y") THEN DELFIL = "/DELETE"
$   IF (DYN.EQS."N") THEN DELFIL = " "
$   IF (DELFIL.EQS."X") THEN GOTO QDEL
$!
$ QSGTSMS:
$ 'WSO' "Warning!  I can no longer backup Save/Get, Tget or message files."
$ EXC = "XX*.*, OO*.*, SC*.*, SG*.*, TS*.*, MS*.*"
$ XSGTSMS:
$!
$ LREVIEW:
$   'WSO' " "
$   'WSO' "This is what you have entered so far :"
$   'WSO' "  1. Tape density is ", DENS
$   'WSO' "  2. User number ",UN
$   IF (REWIND.EQS."/REWIND") THEN -
      'WSO' "  3. Start at beginning of tape (Clobber old data on tape)"
$   IF (REWIND.EQS."/NOREWIND") THEN -
      'WSO' "  3. Start at the end of the tape"
$   IF (LN.EQS."Y") THEN 'WSO' "  4. Type each file name as it is backed up"
$   IF (LN.EQS."N") THEN 'WSO' "  4. No listing of file names"
$   IF (DN.NE.0) THEN 'WSO' "  5. Backup AIPS disk ",DN
$   IF (DN.EQ.0) THEN 'WSO' "  5. Backup all AIPS disks"
$   IF (DELFIL.EQS."/DELETE") THEN 'WSO' "  6. DELETE files after backup"
$   IF (DELFIL.EQS." ") THEN 'WSO' "  6. Do not delete files after backup"
$   'WSO' " "
$   SET NOON
$   QREVIEW:
$     INQUIRE ANS "Enter: 1=backup files, 2=quit"
$     IF (ANS.EQS."2") THEN GOTO FINI
$   IF (ANS.NES."1") THEN GOTO QREVIEW
$!
$ BCKUP:
$ DD1 = ISDISK
$ BLOOP:
$   DISK = "DA0" + F$STRING(DD1)
$   DL = F$TRNLNM(DISK)
$   IF (DL.EQS."") THEN GOTO XBLOOP
$   'WSO' "Backing up AIPS disk ", DD1
$   BACKUP 'REWIND' 'DELFIL' /VERIFY /LABEL=AIPBCK /DENS='DENS' -
      'LOG' 'DISK':*.'HUN'  /EXCLUDE=('EXC')   'MT'DSK'DD1'V1.'HUN'
$   REWIND = "/NOREWIND"
$   DD1 = DD1 + 1
$ IF (DD1.LE.IEDISK) THEN GOTO BLOOP
$ XBLOOP:
$!
$ 'WSO' " "
$ NEXT:
$   INQUIRE ANS "Do you want to backup another disk on the same tape (Y or N)"
$   IF (ANS.EQS."Y") THEN GOTO AGAIN
$   IF (ANS.NES."N") THEN GOTO NEXT
$ FINI:
$ DISMOUNT 'MT'
$ EXIT
