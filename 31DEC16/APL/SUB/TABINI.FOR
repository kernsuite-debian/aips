      SUBROUTINE TABINI (OPCODE, PTYP, VOL, CNO, VER, CATBLK, LUN, NKEY,
     *   NREC, NCOL, DATP, BUFFER, IERR)
C-----------------------------------------------------------------------
C! create/open a table extension file
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1998, 2000, 2007, 2011
C;  Associated Universities, Inc. Washington DC, USA.
C;
C;  This program is free software; you can redistribute it and/or
C;  modify it under the terms of the GNU General Public License as
C;  published by the Free Software Foundation; either version 2 of
C;  the License, or (at your option) any later version.
C;
C;  This program is distributed in the hope that it will be useful,
C;  but WITHOUT ANY WARRANTY; without even the implied warranty of
C;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
C;  GNU General Public License for more details.
C;
C;  You should have received a copy of the GNU General Public
C;  License along with this program; if not, write to the Free
C;  Software Foundation, Inc., 675 Massachusetts Ave, Cambridge,
C;  MA 02139, USA.
C;
C;  Correspondence concerning AIPS should be addressed as follows:
C;         Internet email: aipsmail@nrao.edu.
C;         Postal address: AIPS Project Office
C;                         National Radio Astronomy Observatory
C;                         520 Edgemont Road
C;                         Charlottesville, VA 22903-2475 USA
C-----------------------------------------------------------------------
C   TABINI creates/opens a table extension file.  If a file is created,
C   it is cataloged by a call to CATIO which saves the updated CATBLK.
C   Input:
C      OPCODE   C*4       Operation code, 'READ' => read only,
C                                         'WRIT' => read/write
C      PTYP     I         Physical extension type (eg. 'CC')
C      VOL      I         Disk volume number
C      CNO      I         Catalog slot number
C      CATBLK   I(256)    Catalog block of cataloged file.
C      LUN      I         Logical unit number to use.
C   In/out:
C      VER      I         Version number: (<= 0 => write a new one,
C                         read the latest one), returns one used.
C      NKEY     I         Maximum number of keyword/value pairs
C                         input: used in create, checked on write old
C                         (0 => any, <= actual ok); output: actual
C      NREC     I         Number of logical rec. for create/extend
C                         if 0 on write, reset to 100.
C      NCOL     I         Number of logical columns (does not include
C                         selection column).  Input: used in create,
C                         checked on write old (0=>any, <= actual ok);
C                         output: actual
C      DATP     I(128,2)  DATP(*,1) address pointers (output only)
C                         DATP(*,2) column data type codes. Input:
C                         used in create only; output: actual.
C      BUFFER   I(512)    Work buffer: only 512 now needed
C   Output:
C      IERR     I         Return error code. 0 => OK
C                                        -1 => OK, created new file
C                                         1 => bad input.
C                                         2 => could not find or open
C                                         3 => I/O problem.
C                                         4 => create problem.
C                                         5 => not a table file
C   Usage notes:
C   For sequential access, TABINI leaves pointers for TABIO such that,
C   if IRNO <= 0, reads will begin at the start of the file and writes
C   will begin after the last previous record.  Cataloged file should
C   be marked 'WRIT' if the file is to be created.
C
C   Header record:
C   Each extension file using this system must have the first physical
C   (512 bytes) record containing necessary information. The full table
C   file format is described in Going AIPS.  The user must read this
C   section to understand fully how to use such files.  The header
C   record contains the following:
C
C  I   word(s)          Description
C  1              Number 256-word records now in file
C  2
C  3              Max number rows allowed in current file
C  4
C  5              Number rows (logical records) now in file
C  6
C  7              Number of bytes/value (2 for TA files)
C  8              Number values / logical (# Is / row for TA)
C  9              > 0 => number rows / physical record
C                 < 0 => number physical records / row
C 10              Number logical columns / row
C 11 - 16         Creation date: ZDATE(11), ZTIME(14)
C 17 - 28     H   Physical file name (set on each TABINI call)
C 29 - 30     H   Creation task name
C 31
C 32              Disk number
C 33 - 38         Last access date: ZDATE(33), ZTIME(36)
C 39 - 40     H   Last access task name
C 42              Number logical records to extend file if needed
C 43              Sort order: logical column # of primary sorting
C 44              Sort order: logical column # of secondary sorting
C                      0 => unknown, < 0 => descending order
C 45              Disk record number for column data pointers (2)
C 46              Disk record number for row selection strings (3)
C 47              Disk record number for 1st record of titles (5)
C 48              Disk record number for 1st record of units
C 49              Disk record number for 1st record of keywords
C 50              Disk record number for 1st record of table data
C 51              DATPTR (row selection column)
C 52              Maximum number of keyword/value pairs allowed
C 53              Current number of keyword/value pairs in file
C 54 - 56         "*AIPS TABLE*" packed string to verify that table.
C 57 - 59
C 60              If 1 then then table cannot be written as FITS ASCII
C 61              Number of selection strings now in file
C 62              Next available R   address for a selection string
C 63              First R   address of selection string 1
C 64              First R   address of selection string 2
C 65              First R   address of selection string 3
C 66              First R   address of selection string 4
C 67              First R   address of selection string 5
C 68              First R   address of selection string 6
C 69              First R   address of selection string 7
C 70              First R   address of selection string 8
C********** for TABIO / TABINI use only **********
C 71              IOP : 1 => read, 2 => writ
C 72              Number I   words per logical record
C 73              Current table row physical record in BUFFER
C 74
C 75              Current table row logical record in BUFFER
C 76
C 77              Type of current record in BUFFER
C 78              Current control physical record number in BUFFER
C 79              Current control logical record number in BUFFER
C 80              Type of current control record in BUFFER
C 81              LUN
C 82              FTAB pointer of open file
C***********
C 83 -100         Reserved
C***********
C101 -128     H   Table title
C129 -256         lookup table as COLPTR(logical column) = phys column
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, PTYP*2
      INTEGER   VOL, CNO, VER, CATBLK(256), LUN, NKEY, NREC, NCOL,
     *   DATP(128,2), BUFFER(512), IERR
C
      LOGICAL   EQUAL, OLD, TABLE, EXIST, FITASC, FORGOT
      CHARACTER ATLAB*12, CHTEMP*2, PHNAME*48, OP*4, FNAME*12, FCLASS*6,
     *   FTYPE*2, STATUS*4
      INTEGER   IND, IOP, NEXT, I, J, L, NVER, IP, LP, LREC, NLPR,
     *   IER, JERR, I4T, KREC, ISIZE, LSIZE, FSEQ, FUSID
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      DATA ATLAB /'*AIPS TABLE*'/
C-----------------------------------------------------------------------
      OLD = .TRUE.
      TABLE = .TRUE.
      EXIST = .FALSE.
      IND = 0
C                                       Check OPCODE
      IOP = 0
      IERR = 1
      IF (OPCODE.EQ.'READ') IOP = 1
      IF (OPCODE.EQ.'WRIT') IOP = 2
      IF (IOP.EQ.0) THEN
         WRITE (MSGTXT,1010) OPCODE
         GO TO 990
         END IF
C                                       Check if file exists.
      CALL FXHDEX (CATBLK)
      NEXT = KIEXTN + 1
      IERR = 2
      DO 20 I = 1,KIEXTN
         CALL H2CHR (2, 1, CATBLK(KHEXT+I-1), CHTEMP)
         IF (PTYP.EQ.CHTEMP) THEN
            NEXT = I
            GO TO 50
         ELSE
            IF ((CHTEMP.EQ.' ') .OR. (CATBLK(KHEXT+I-1).EQ.0)) NEXT =
     *         MIN (NEXT, I)
            END IF
 20      CONTINUE
C                                       Catalog block full?
      IF (NEXT.GT.KIEXTN) THEN
         MSGTXT = 'TABINI: EXTENSION FILE LIST FULL'
         GO TO 990
         END IF
C                                       Some old version exists.
 50   CONTINUE
         NVER = CATBLK(KIVER+NEXT-1)
         IF (VER.LE.0) VER = NVER + IOP - 1
C                                       See if requested version exists
         CALL ISTAB (PTYP, VOL, CNO, VER, LUN, BUFFER, TABLE, EXIST,
     *      FITASC, JERR)
         IF (JERR.NE.0) THEN
            IERR = 3
            MSGTXT = 'TABINI: I/O ERROR FROM ISTAB ON ' // PTYP
            GO TO 990
            END IF
C                                       Forgotten version?
C                                       Delete forgotten file if
C                                       writing, keep if reading.
         FORGOT = EXIST .AND. ((VER.GT.NVER) .OR. (JERR.NE.0))
         EXIST = EXIST .AND. (JERR.EQ.0)
         IF (EXIST .AND. ((OPCODE.EQ.'READ') .OR. (.NOT.FORGOT)))
     *      GO TO 140
C                                       None exist: ok on write only
            IF (OPCODE.EQ.'READ') THEN
               WRITE (MSGTXT,1060) PTYP, VER
               GO TO 990
               END IF
C                                       write any ver
C            IF (VER.GT.NVER) VER = NVER + 1
            IF (VER.GT.46655) THEN
               MSGTXT = 'CANNOT CREATE MORE THAN 46655 VERSIONS OF' //
     *            ' AN EXT. FILE'
               GO TO 990
               END IF
C                                       CREATE new file.
C                                       Parse the data structure
      CALL FILL (256, 0, BUFFER)
      CALL FILL (128, 0, DATP)
C                                       Add AIPS table label
      CALL CHR2H (12, ATLAB, 1, BUFFER(54))
      IP = 1
      LP = 1
      DO 120 I = 1,7
         IF (I.EQ.6) GO TO 120
         DO 110 J = 1,NCOL
C                                       Found column of right type
            IF (MOD(DATP(J,2), 10).EQ.I) THEN
C                                       DATP(J,1) = Pointer in array
C                                       of appropriate type.
               DATP(J,1) = IP
               BUFFER(128+J) = LP
               LP = LP + 1
C                                       Get length of array.
               L = DATP(J,2) / 10
               IF (I.EQ.3) L = (L-1) / 4 + 1
               IF (I.EQ.7) L = (L-1) / NBITWD + 1
               IF (DATP(J,2).LT.10) L = 0
C                                       Set pointer for next entry.
               IP = IP + L
C                                       If L>1 and I .NE. 3 then the
C                                       file cannot be written as
C                                       FITS ASCII
               IF ((L.GT.1) .AND. (I.NE.3)) BUFFER(60) = 1
               END IF
 110        CONTINUE
C                                       Set pointer for next type.
         IF (I.EQ.1) IP = (IP-1) * NWDPDP + 1
 120     CONTINUE
C                                       error in data types
      IF (LP.EQ.NCOL+1) GO TO 130
         WRITE (MSGTXT,1120)
         IERR = 1
         GO TO 990
C                                       select column pointers
 130  LREC = 2 * IP
      BUFFER(128+LP) = LP
      DATP(LP,1) = IP
      DATP(LP,2) = 9
C                                       record pointers
      BUFFER(45) = 2
      BUFFER(46) = 3
      BUFFER(47) = 5
      BUFFER(48) = 6 + (NCOL - 1) / (256 / 6)
      BUFFER(49) = BUFFER(48) + 1 + (NCOL - 1) / (256 / 2)
      BUFFER(50) = BUFFER(49) + 1 + (NKEY - 1) / (256 / 5)
      KREC = BUFFER(50) - 1
C                                       file size
      IF ((NREC.LE.0) .OR. (NREC.GT.10000)) NREC = 100
      NLPR = 512.0 / LREC
      IF (NLPR.LE.0) NLPR = -(LREC / 512.0 + 0.9999)
      IF (NLPR.GT.0) ISIZE = KREC + 1 + (NREC-1)/NLPR
      IF (NLPR.LE.0) ISIZE = KREC - NLPR * NREC
      IERR = 4
      CALL ZPHFIL (PTYP, VOL, CNO, VER, PHNAME, IER)
      CALL CHR2H (48, PHNAME, 1, BUFFER(17))
C                                       Delete forgotten old file on
C                                       write.
      IF (FORGOT .AND. (OPCODE.EQ.'WRIT')) THEN
         WRITE (MSGTXT,1125) PTYP, VER
         CALL MSGWRT (6)
         CALL ZDESTR (VOL, PHNAME, IER)
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1126) IER, PTYP, VER
            GO TO 990
            END IF
         END IF
C                                       Create file
      CALL ZCREAT (VOL, PHNAME, ISIZE, .FALSE., LSIZE, IER)
      IF (IER.EQ.0) OLD = .FALSE.
      IF (IER.LE.1) GO TO 140
         WRITE (MSGTXT,1130) IER, PTYP, VER
         IF (IER.EQ.5) WRITE (MSGTXT,1131) PTYP, VER, VOL
         GO TO 990
 140  ISIZE = LSIZE
C                                       Catalog ext. file
      IF ((.NOT.OLD) .OR. FORGOT) THEN
C                                       Correct max table number in
C                                       header .
         CALL CHR2H (2, PTYP, 1, CATBLK(KHEXT+NEXT-1))
         CATBLK(KIVER+NEXT-1) = MAX (VER, CATBLK(KIVER+NEXT-1))
         IF (OPCODE.EQ.'WRIT') THEN
            CALL CATIO ('UPDT', VOL, CNO, CATBLK, 'REST', BUFFER(257),
     *         IER)
C                                       File being read; some trickery
C                                       is called for here.
         ELSE
            CALL H2CHR (12, KHIMNO, CATBLK(KHIMN), FNAME)
            CALL H2CHR (6, KHIMCO, CATBLK(KHIMC), FCLASS)
            CALL H2CHR (2, KHPTYO, CATBLK(KHPTY), FTYPE)
            FSEQ = CATBLK(KIIMS)
            FUSID = CATBLK(KIIMU)
            STATUS = 'CLRD'
            CALL CATDIR ('CSTA', VOL, CNO, FNAME, FCLASS, FSEQ, FTYPE,
     *         FUSID, STATUS, BUFFER(257), IER)
            STATUS = 'READ'
            IF (IER.EQ.0)
     *         CALL CATIO ('UPDT', VOL, CNO, CATBLK, STATUS,
     *            BUFFER(257), IER)
            END IF
         IF (IER.NE.0) THEN
            WRITE (MSGTXT,1140) IER, PTYP, VER
            GO TO 990
            END IF
         END IF
C                                       OPEN file.
      CALL ZPHFIL (PTYP, VOL, CNO, VER, PHNAME, IER)
      CALL CHR2H (48, PHNAME, 1, BUFFER(17))
      EQUAL = IOP.EQ.2
      IERR = 2
      CALL ZOPEN (LUN, IND, VOL, PHNAME, .FALSE., EQUAL, .TRUE., IER)
      IF (IER.GT.0) THEN
         IND = 0
         WRITE (MSGTXT,1200) IER, PTYP, VER
         GO TO 990
         END IF
C                                       Message about recovering old file
      IF (FORGOT .AND. (OPCODE.EQ.'READ')) THEN
         WRITE (MSGTXT,1201) PTYP, VER
         CALL MSGWRT (6)
         END IF
C                                       If new file, fill in header.
      IF (OLD) GO TO 220
         BUFFER(1) = ISIZE
C                                       determine # log. max in file
         IF (NLPR.GE.0) THEN
            BUFFER(3) = NLPR * (ISIZE - KREC)
         ELSE
            BUFFER(3) = (ISIZE - KREC) / ABS(NLPR)
            END IF
         BUFFER(5) = 0
         CALL CHR2H (6, TSKNAM, 1, BUFFER(29))
         BUFFER(7) = 2
         BUFFER(8) = DATP(NCOL+1,1)
         BUFFER(9) = NLPR
         BUFFER(10) = NCOL
         CALL ZDATE (BUFFER(11))
         CALL ZTIME (BUFFER(14))
         BUFFER(32) = VOL
         BUFFER(42) = NREC
         BUFFER(51) = BUFFER(8)
         LP = 256 / 5
         BUFFER(52) = LP * (BUFFER(50) - BUFFER(49))
         BUFFER(62) = 1
         BUFFER(63) = 1
         I = 28
         CALL RFILL (I, HBLANK, BUFFER(101))
C                                       Write header.
         IERR = 3
         OP = 'WRIT'
         CALL ZFIO ('WRIT', LUN, IND, 1, BUFFER, IER)
         IF (IER.NE.0) GO TO 980
         CALL ZFIO ('WRIT', LUN, IND, BUFFER(45), DATP, IER)
         IF (IER.NE.0) GO TO 980
C                                       write null records
         CALL RFILL (256, HBLANK, BUFFER(257))
         I4T = BUFFER(46)
         IP = BUFFER(50) - BUFFER(46)
         DO 215 I = 1,IP
            CALL ZFIO ('WRIT', LUN, IND, I4T, BUFFER(257), IER)
            IF (IER.NE.0) GO TO 980
            I4T = I4T + 1
 215        CONTINUE
         GO TO 230
C                                       Read header, pointers
 220  CONTINUE
         IERR = 3
         OP = 'READ'
         CALL ZFIO ('READ', LUN, IND, 1, BUFFER, IER)
         IF (IER.NE.0) GO TO 980
C                                       Not table format
         IF (.NOT.TABLE) THEN
            IERR = 5
            MSGTXT = 'TABINI: FILE NOT A TABLE FILE'
            GO TO 990
            END IF
         CALL ZFIO ('READ', LUN, IND, BUFFER(45), DATP, IER)
         IF (IER.NE.0) GO TO 980
         CALL CHR2H (48, PHNAME, 1, BUFFER(17))
C                                       If write - fill info
 230  IF (OPCODE.EQ.'WRIT') THEN
         BUFFER(32) = VOL
         CALL ZDATE (BUFFER(33))
         CALL ZTIME (BUFFER(36))
         CALL CHR2H (6, TSKNAM, 1, BUFFER(39))
         IF ((NREC.LE.0) .OR. (NREC.GT.10000)) NREC = 100
         BUFFER(42) = NREC
C                                       Check structure parms
         IF (OLD) THEN
            IF ((NKEY.GT.BUFFER(52)) .OR. (NCOL.GT.BUFFER(10))) THEN
               WRITE (MSGTXT,1230) NKEY, BUFFER(52), NCOL, BUFFER(10)
               IERR = 1
               GO TO 990
               END IF
            END IF
         END IF
C                                       Check record length.
 240  NLPR = BUFFER(9)
C                                       return parms
      NKEY = BUFFER(52)
      NCOL = BUFFER(10)
C                                       set up I/O
      BUFFER(71) = IOP
      BUFFER(72) = BUFFER(8)
      BUFFER(73) = 0
      BUFFER(75) = BUFFER(5)
      IF (IOP.EQ.1) BUFFER(75) = 0
      BUFFER(77) = -1
      BUFFER(78) = 0
      BUFFER(79) = 0
      BUFFER(80) = -1
      BUFFER(81) = LUN
      BUFFER(82) = IND
      IERR = 0
      IF (.NOT.OLD) IERR = -1
      GO TO 999
C                                       Error
 980  WRITE (MSGTXT,1980) OP, IER
 990  CALL MSGWRT (6)
      IF (IND.GT.0) CALL ZCLOSE (LUN, IND, IER)
      IF (.NOT.OLD) THEN
         CALL H2CHR (48, 1, BUFFER(17), PHNAME)
         CALL ZDESTR (VOL, PHNAME, IER)
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TABINI: UNKNOWN OPCODE: ',A4)
 1060 FORMAT ('TABINI: REQUESTED ',A2,' FILE ',I3,' DOES NOT EXIST')
 1120 FORMAT ('TABINI: SOME ERROR IN LIST OF TABLE DATA TYPES')
 1125 FORMAT ('TABINI: Deleting old ',A,' table version',I4)
 1126 FORMAT ('TABINI: ERROR ',I3,' DELETING OLD ',A,
     *   ' TABLE VERSION',I4)
 1130 FORMAT ('TABINI: ERROR',I3,' CREATING FILE ',A2,' NO ',I3)
 1131 FORMAT ('TABINI: PROHIBITED FROM CREATING FILE ',A2,' NO ',I3,
     *   ' ON DISK',I3)
 1140 FORMAT ('TABINI: ERROR',I3,' CATALOGING FILE ',A2,' NO ',I3)
 1200 FORMAT ('TABINI: ERROR',I3,' OPENING FILE ',A2,' NO. ',I3)
 1201 FORMAT ('TABINI: Recovering old ',A,' table version',I4)
 1230 FORMAT ('TABINI: KEY COUNTS',2I7,' OR COLS',2I7,' DON''T MATCH')
 1980 FORMAT ('TABINI: ',A4,' ERROR',I5)
      END
