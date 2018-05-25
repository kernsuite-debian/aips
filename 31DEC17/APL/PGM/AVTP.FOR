      PROGRAM AVTP
C-----------------------------------------------------------------------
C! Task to position a tape.
C# Tape,  IO-util
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2004
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
C   AVTP is an AIPS system task to position a tape.
C   AIPS Adverbs:
C      INTAPE    R      Tape drive number: 0 => 1.
C      OPCODE    R      Desired operation: 'REWI' rewind
C                       'EOT '  advance to end of tape
C                       'AVFI'  advance or back files
C      NFILES    R      # files for AVFI: > 0 -> advance
C                          = 0 -> back to start of present file
C                          < 0 -> back even further.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, TNAME*48, OPCODS(3)*4, OPCODE*4
      INTEGER   NOPCOD, ISCR(256),NPARMS, IERR, IROUND, IOP, IFILES,
     *   TVOL, TLUN, TIND, IRET, I
      REAL      TAPEIN, XFILES
      HOLLERITH XOPCOD
      LOGICAL   T, F, RQUICK
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      COMMON /INPRMS/ TAPEIN, XOPCOD, XFILES
      DATA T, F /.TRUE.,.FALSE./
      DATA PRGM /'AVTP  '/
      DATA NOPCOD, OPCODS /3, 'REWI','AVFI','EOT '/
C-----------------------------------------------------------------------
C                                       Init
      CALL ZDCHIN (.TRUE., ISCR)
      NPARMS = 3
      CALL GTPARM (PRGM, NPARMS, RQUICK, TAPEIN, ISCR, IERR)
      IRET = 8
      IF (IERR.NE.0) GO TO 980
C                                       Tapes allowed?
 10   IF ((NPOPS.LE.NINTRN) .AND. (ISBTCH.NE.32000)) GO TO 20
         WRITE (MSGTXT,1010)
         GO TO 970
C                                       Check inputs
 20   IFILES = IROUND (XFILES)
      TVOL = IROUND (TAPEIN)
      IF ((TVOL.EQ.0) .OR. (NTAPED.EQ.1)) TVOL = 1
      IF ((TVOL.GT.0) .AND. (TVOL.LE.NTAPED)) GO TO 30
         WRITE (MSGTXT,1020) TVOL
         GO TO 970
C                                       Change XOPCOD -> Char.
 30   CALL H2CHR (4, 1, XOPCOD, OPCODE)
      DO 35 IOP = 1,NOPCOD
         IF (OPCODE.EQ.OPCODS(IOP)) GO TO 40
 35      CONTINUE
      WRITE (MSGTXT,1035) OPCODE
      GO TO 970
C                                       Resume AIPS
 40   IF (RQUICK) CALL RELPOP (0, ISCR, IERR)
C                                       Open tape
      TLUN = 129 - TVOL
      CALL ZPHFIL ('MT', TVOL, 1, 1, TNAME, IERR)
      CALL ZTPOPN (TLUN, TIND, TVOL, TNAME, 'READ', IERR)
      IF (IERR.EQ.0) GO TO 50
         WRITE (MSGTXT,1040) IERR
         CALL MSGWRT (8)
         GO TO 990
C                                       Branch to operation
 50   GO TO (100, 200, 300), IOP
C                                       REWInd
 100  CONTINUE
         I = 1
         CALL ZTAPE (OPCODS(1), TLUN, TIND, I, IERR)
         GO TO 900
C                                       Advance files
 200  IF (IFILES.LE.0) GO TO 210
         CALL ZTAPE ('ADVF', TLUN, TIND, IFILES, IERR)
         GO TO 900
C                                       Back up files
 210  CONTINUE
         IFILES = 1 - IFILES
         CALL ZTAPE ('BAKF', TLUN, TIND, IFILES, IERR)
         GO TO 900
C                                       Advance to EOT
 300  CONTINUE
         IFILES = 0
         CALL ZTAPE ('AEOI', TLUN, TIND, IFILES, IERR)
         IF (IERR.EQ.0) THEN
            I = ABS (IFILES) - 1
            IF (IFILES.GT.0) WRITE (MSGTXT,1300) I
            IF (IFILES.LT.0) WRITE (MSGTXT,1301) I
            IF (IFILES.EQ.0) MSGTXT = 'Advanced to EOI after ' //
     *         'unknown number of files'
            CALL MSGWRT (3)
            END IF
         GO TO 900
C                                       Close down
 900  IF (IERR.EQ.0) IRET = 0
      IF (IERR.EQ.0) GO TO 910
         WRITE (MSGTXT,1900) IERR
         CALL MSGWRT (8)
 910  CALL ZTPCLS (TLUN, TIND, IERR)
      GO TO 990
C                                       User errors
 970  CALL MSGWRT (8)
 980  RQUICK = .FALSE.
C                                       AIPS standard quit
 990  CALL DIETSK (IRET, RQUICK, ISCR)
C
 999  STOP
C-----------------------------------------------------------------------
 1010 FORMAT ('TAPES ARE NOT ALLOWED IN BATCH JOBS')
 1020 FORMAT ('ILLEGAL TAPE DRIVE NUMBER',I7)
 1035 FORMAT ('UNRECOGNIZED OPCODE = ''',A4,'''')
 1040 FORMAT ('TAPE OPEN ERROR',I7)
 1300 FORMAT ('Advanced to end-of-information after file',I5)
 1301 FORMAT ('Advanced to end-of-information after skipping',I5,'
     *   files')
 1900 FORMAT ('TAPE IO ERROR',I7)
      END
