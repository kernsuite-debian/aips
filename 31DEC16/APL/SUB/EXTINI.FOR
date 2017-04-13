      SUBROUTINE EXTINI (OPCODE, PTYP, VOL, CNO, VER, CATBLK, LUN, IND,
     *   LREC, NREC, BUFFER, IERR)
C-----------------------------------------------------------------------
C! creates and/or opens an extension file of the EXTINI/EXTIO type
C# EXT-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1997, 2000
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
C   EXTINI creates/opens an extension file.  If a file is created it
C   is catalogd by a call to CATIO which saves the updated CATBLK.
C   Inputs:
C      OPCODE   C*4      Operation code, 'READ' => read only,
C                                          'WRIT' => read/write
C      PTYP     C*2      Physical extension type (eg. 'CC')
C      VOL      I        Volume number
C      CNO      I        Catalog slot number
C      LUN      I        Logical unit number to use.
C      NREC     I        Number of logical rec. for create/extend
C   In/out:
C      VER      I        in: Version number: (<= 0 => write a new one,
C                            read the latest one)
C                        out: Version number used.
C      CATBLK   I(256)   Catalog block of cataloged file, ext info is
C                        updated if necessary.
C      LREC     I        in: Record length in units of REALs (write new)
C                        out: Logical record length (in units of REALs)
C                             for read/write old files
C      BUFFER   I(*)     Work buffer, at least 1024 bytes in size,
C                        more if logical record longer than 512 bytes
C                        out: Header info. for EXTIO
C   Output:
C      IND      I        FTAB pointer.
C      IERR     I        Return error code. 0 => OK
C                                         1 => bad input.
C                                         2 => could not find or open
C                                         3 => create/I/O problem.
C   Useage notes:
C   For sequential access, EXTINI leaves pointers for EXTIO such that
C   if IRNO .le. 0 reads will begin at the start of the file and writes
C   will begin after the last previous record.
C   File should be marked 'WRIT' if the file is to be created.
C
C   Header record:
C   Each extension file using this system must have the first physical
C   (512 bytes) record containing necessary information.  In addition
C   space in this first record not reserved can be used for other
C   purposes.  The header record contains the following:
C
C   I   word(s)                   description
C      1           # 512-byte records in the existing file
C      2           # logical records to extend the file when req.
C      3           max. # of logical records
C      4           current number of logical records
C      5           # bytes per value
C      6           # values per logical record.
C      7           # of logical records per physical record, if neg then
C                  the # of physical records per logical record.
C   8 - 10         Creation task name (2 char per word)
C  11 - 16         Creation date, time
C  17 - 28         File name (packed character string)
C     29           Volumn number on which file resides.
C  30 - 32         Last write-access task (2 char per word)
C  33 - 38         Last write-access time,date
C  39 - 56         reserved. (53-56 used by EXTIO:
C                             53 = # I   words per logical record.
C                             54 = IOP sent to EXTINI
C                             55 = current physical record no.
C                                  (doesn't include header rec.)
C                             56 = current logical rec. no.
C  57 -256         Available for use.
C  Programmer: W. D. Cotton, Sept. 1981.
C-----------------------------------------------------------------------
      CHARACTER OPCODE*4, PTYP*2
      INTEGER   VOL, CNO, VER, CATBLK(256), LUN, IND, LREC,
     *   NREC, BUFFER(*), IERR
C
      CHARACTER CHTEMP*2, PHNAME*48
      INTEGER   IOP, IER, NEXT, NVER, I, NLPR, ISIZE, LSIZE
      LOGICAL   T, F, EQUAL, OLD
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
      OLD = T
      IND = 0
C                                       Check OPCODE
      IOP = 0
      IERR = 1
      IF (OPCODE.EQ.'READ') IOP = 1
      IF (OPCODE.EQ.'WRIT') IOP = 2
      IF (IOP.LE.0) THEN
         WRITE (MSGTXT,1010) OPCODE
         GO TO 990
         END IF
C                                       check header format
      CALL FXHDEX (CATBLK)
C                                       Check if file exists.
 10   NEXT = KIEXTN + 1
      IERR = 2
      DO 20 I = 1,KIEXTN
         CALL H2CHR (2, 1, CATBLK(KHEXT+I-1), CHTEMP)
         EQUAL = PTYP.EQ.CHTEMP
         IF (EQUAL) NEXT = I
         IF (EQUAL) GO TO 50
            EQUAL = (CHTEMP.EQ.'  ') .OR. (CATBLK(KHEXT+I-1).LE.0)
            IF (EQUAL) NEXT = MIN (NEXT, I)
 20      CONTINUE
C                                       Catalog block full
      IF (NEXT.LE.KIEXTN) GO TO 30
         WRITE (MSGTXT,1020)
         GO TO 990
C                                       None exist: ok on write only
 30   CONTINUE
         VER = 1
         IF (IOP.EQ.2) GO TO 100
 35         WRITE (MSGTXT,1035) PTYP, VER
            GO TO 990
C                                       Some old version exists.
 50   CONTINUE
         NVER = CATBLK(KIVER+NEXT-1)
         IF ((VER.GT.NVER) .AND. (IOP.EQ.1)) GO TO 35
         IF (VER.LE.0) VER = NVER + IOP - 1
C                                       See if requested version exists
         IF (VER.LE.NVER) GO TO 200
            VER = NVER + 1
            IF (VER.LE.46655) GO TO 100
               WRITE (MSGTXT,1050)
               GO TO 990
C                                       CREATE new file.
 100  CALL CHR2H (2, PTYP, 1, CATBLK(KHEXT+NEXT-1))
      CATBLK(KIVER+NEXT-1) = MAX (VER, CATBLK(KIVER+NEXT-1) + 1)
      NLPR = 256.0 / LREC
      IF (NLPR.LE.0) NLPR = -(LREC/256.0 + 0.999)
      IF (NLPR.GT.0) ISIZE = 2 + (NREC-1) / NLPR
      IF (NLPR.LE.0) ISIZE = 1 - NLPR * NREC
      IERR = 3
      CALL ZPHFIL (PTYP, VOL, CNO, VER, PHNAME, IER)
      CALL CHR2H (48, PHNAME, 1, BUFFER(17))
      CALL ZCREAT (VOL, PHNAME, ISIZE, F, LSIZE, IER)
      IF (IER.EQ.0) OLD = F
      IF (IER.EQ.0) ISIZE = LSIZE
      IF (IER.LE.1) GO TO 110
         WRITE (MSGTXT,1100) IER, PTYP, VER
         IF (IER.EQ.5) WRITE (MSGTXT,1101) PTYP, VER, VOL
         GO TO 990
C                                       Catalog ext. file
 110  CALL CATIO ('UPDT', VOL, CNO, CATBLK, 'REST', BUFFER(257), IER)
      IF (IER.EQ.0) GO TO 200
         WRITE (MSGTXT,1110) IER, PTYP, VER
         GO TO 990
C                                       OPEN file.
 200  CALL ZPHFIL (PTYP, VOL, CNO, VER, PHNAME, IER)
      EQUAL = IOP.EQ.2
      IERR = 2
      CALL ZOPEN (LUN, IND, VOL, PHNAME, F, EQUAL, T, IER)
      IF (IER.LE.0) GO TO 210
         IND = 0
         WRITE (MSGTXT,1200) IER, PTYP, VER
         GO TO 990
C                                       If new file, fill in header.
 210  IF (OLD) GO TO 220
C                                       determine # log./physical rec.
         BUFFER(1) = ISIZE
         BUFFER(2) = NREC
         BUFFER(3) = NLPR * (ISIZE-1)
         IF (NLPR.LT.0) BUFFER(3) = (ISIZE-1) / ABS (NLPR)
         BUFFER(4) = 0
         BUFFER(5) = 2
         BUFFER(6) = LREC
         BUFFER(7) = NLPR
         CALL CHR2H (6, TSKNAM, 1, BUFFER(8))
         CALL ZDATE (BUFFER(11))
         CALL ZTIME (BUFFER(14))
         BUFFER(29) = VOL
C                                       Write header.
         CALL ZFIO ('WRIT', LUN, IND, 1, BUFFER, IER)
         IF (IER.EQ.0) GO TO 230
            WRITE (MSGTXT,1210) 'WRIT', IER
            GO TO 990
C                                       Read header
 220  CONTINUE
         CALL ZFIO ('READ', LUN, IND, 1, BUFFER, IER)
         CALL CHR2H (48, PHNAME, 1, BUFFER(17))
         IF (IER.EQ.0) GO TO 230
            WRITE (MSGTXT,1210) 'READ', IER
            GO TO 990
C                                       If write - fill info
 230  IF (IOP.EQ.1) GO TO 240
         BUFFER(2) = NREC
         CALL CHR2H (6, TSKNAM, 1, BUFFER(30))
         CALL ZDATE (BUFFER(33))
         CALL ZTIME (BUFFER(36))
C                                       Check record length.
 240  IF (.NOT. (LREC.GT.BUFFER(6))) GO TO 300
         WRITE (MSGTXT,1240) LREC, BUFFER(6), BUFFER(5)
         IERR = 1
         GO TO 990
C                                       Prepare info for I/O
 300  LREC = BUFFER(6)
      BUFFER(53) = BUFFER(5) * BUFFER(6) / 2
      BUFFER(54) = IOP
      BUFFER(55) = 0
      BUFFER(56) = BUFFER(4)
      IF (IOP.EQ.1) BUFFER(56) = 0
      IERR = 0
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
      IF (IND.GT.0) CALL ZCLOSE (LUN, IND, IER)
      IF (.NOT.OLD) CALL ZDESTR (VOL, PHNAME, IER)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('EXTINI: UNKNOWN OPCODE: ',A4)
 1020 FORMAT ('EXTINI: EXTENSION FILE LIST FULL')
 1035 FORMAT ('EXTINI: REQUESTED ',A2,' FILE ',I3,' DOES NOT EXIST')
 1050 FORMAT ('CANNOT CREATE MORE THAN 46655 VERSIONS OF AN EXT. FILE')
 1100 FORMAT ('EXTINI: ERROR',I3,' CREATING FILE ',A2,' NO ',I3)
 1101 FORMAT ('EXTINI: PROHIBITED FROM CREATING FILE ',A2,' NO ',I3,
     *   ' ON DISK',I3)
 1110 FORMAT ('EXTINI: ERROR',I3,' CATALOGING FILE ',A2,' NO ',I3)
 1200 FORMAT ('EXTINI: ERROR',I3,' OPENING FILE ',A2,' NO. ',I3)
 1210 FORMAT ('EXTINI: ',A4,' ERROR',I3)
 1240 FORMAT ('EXTINI: FILES CONFLICT,LREC=',2I5,' BP=',2I2)
      END
