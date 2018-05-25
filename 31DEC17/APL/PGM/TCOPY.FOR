      PROGRAM TCOPY
C-----------------------------------------------------------------------
C! Tape to tape copy with some disk FITS support
C# TAPE UTILITY FITS
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2001, 2005
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
C   Copy a tape of arbitrary files to another tape.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INFILE*48, OUTFIL*48, TNAME*48, ERRTAP*8
      INTEGER   INTAPE, NSKIP, NCOPY, OUTTAP, IERR, TLUNI, TLUNO, TINDI,
     *   TINDO, LOOP, IFILES, I, JERR, KERR, ISCR(256), ICOUNT, LCOUNT
      DOUBLE PRECISION BCOUNT(1000), TOTAL, BYTES
      LOGICAL   DOEOT
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
      DATA PRGM /'TCOPY '/
C-----------------------------------------------------------------------
C                                       Startup
      CALL TCSTRT (PRGM, INTAPE, NSKIP, NCOPY, INFILE, OUTTAP, DOEOT,
     *   OUTFIL, IERR)
      IF (IERR.NE.0) GO TO 990
      IFILES = 0
      TOTAL = 0.0D0
C                                       Open input
      ERRTAP = 'INPUT'
      IF (INFILE.EQ.' ') THEN
         TLUNI = 129 - INTAPE
         CALL ZPHFIL ('MT', INTAPE, 1, 1, TNAME, IERR)
      ELSE
         TLUNI = 24
         TNAME = INFILE
         NCOPY = 1
         NSKIP = 0
         END IF
      CALL ZTPOPN (TLUNI, TINDI, INTAPE, TNAME, 'READ', IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, ERRTAP
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Position input
      IF (TLUNI.GT.30) THEN
         IF (NSKIP.GT.0) THEN
            CALL ZTAPE ('ADVF', TLUNI, TINDI, NSKIP, IERR)
            IF (IERR.NE.0) GO TO 800
C                                       Back up files
         ELSE IF (NSKIP.LT.0) THEN
            NSKIP = 1 - NSKIP
            CALL ZTAPE ('BAKF', TLUNI, TINDI, NSKIP, IERR)
            IF (IERR.NE.0) GO TO 800
            END IF
         END IF
C                                       Open output
      ERRTAP = 'OUTPUT'
      IF (OUTFIL.EQ.' ') THEN
         TLUNO = 129 - OUTTAP
         CALL ZPHFIL ('MT', OUTTAP, 1, 1, TNAME, IERR)
      ELSE
         TLUNO = 25
         TNAME = OUTFIL
         DOEOT = .FALSE.
         NCOPY = 1
         END IF
      CALL ZTPOPN (TLUNO, TINDO, OUTTAP, TNAME, 'WRIT', IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1000) IERR, ERRTAP
         CALL MSGWRT (8)
         GO TO 900
         END IF
C                                       Position output
      IF ((TLUNO.GT.30) .AND. (DOEOT)) THEN
         IFILES = 0
         CALL ZTAPE ('AEOI', TLUNO, TINDO, IFILES, IERR)
         IF (IERR.EQ.0) THEN
            I = ABS (IFILES) - 1
            IF (IFILES.GT.0) WRITE (MSGTXT,1010) I
            IF (IFILES.LT.0) WRITE (MSGTXT,1011) I
            IF (IFILES.NE.0) CALL MSGWRT (3)
            END IF
         IF (IERR.NE.0) GO TO 800
         END IF
C                                       Copy
      IFILES = 0
      DO 50 LOOP = 1,NCOPY
         CALL FCOPY (TLUNI, TLUNO, TINDI, TINDO, BCOUNT, LCOUNT, IERR)
C                                       Done?
         IF (IERR.LT.0) THEN
            IERR = 0
            GO TO 900
            END IF
         IF (IERR.NE.0) GO TO 900
C                                       Count files, bytes copied
         DO 40 ICOUNT = 1,LCOUNT
            IFILES = IFILES + 1
            BYTES = BCOUNT(ICOUNT) / 1.0D6
            WRITE (MSGTXT,1020) IFILES, BYTES
            CALL MSGWRT (4)
C                                       Sum byte count
            TOTAL = TOTAL + BYTES
 40         CONTINUE
 50      CONTINUE
      GO TO 900
C                                       Position error
 800  WRITE (MSGTXT,1800) IERR, ERRTAP
      CALL MSGWRT (8)
C                                       Close down
 900  CALL ZTPCLS (TLUNI, TINDI, JERR)
      IF (JERR.NE.0) ERRTAP = 'INPUT'
C                                       BAKF tape to where it already is
C                                       to encourage the OS to not write
C                                       an extra EOF on the tape
      IF ((IFILES.GT.0) .AND. (TLUNO.GT.30)) THEN
         I = 1
         CALL ZTAPE ('BAKF', TLUNO, TINDO, I, KERR)
         IF (KERR.NE.0) ERRTAP = 'OUTPUT'
         END IF
      CALL ZTPCLS (TLUNO, TINDO, KERR)
      IF (KERR.NE.0) ERRTAP = 'OUTPUT'
      IF ((JERR.NE.0) .OR. (KERR.NE.0)) THEN
         IERR = MAX (JERR, KERR)
         WRITE (MSGTXT,1900) IERR, ERRTAP
         END IF
C                                       Tell number of files copied
      IF (IFILES.GT.0) THEN
         WRITE (MSGTXT,1901) IFILES, TOTAL
      ELSE
         MSGTXT = 'WARNING: NO files copied'
         END IF
      CALL MSGWRT (4)
      GO TO 990
C                                       AIPS standard quit
 990  CALL DIE (IERR, ISCR)
C
 999  STOP
C-----------------------------------------------------------------------
 1000 FORMAT ('TAPE OPEN ERROR',I7,' ON ',A)
 1010 FORMAT ('Advanced output to end-of-information after file',I6)
 1011 FORMAT ('Advanced output to end-of-information after ',
     *   'skipping',I6,' files ')
 1020 FORMAT ('Copied file ',I6,', ',F10.3,' Mbyte')
 1800 FORMAT ('TAPE I/O ERROR',I7,' ON ',A)
 1900 FORMAT (' ERROR',I7,' CLOSING ',A,' TAPE')
 1901 FORMAT ('Copied ',I6,' files totaling ',F10.3,' Mbyte')
      END
      SUBROUTINE TCSTRT (PRGM, INTAPE, NSKIP, NCOPY, INFILE, OUTTAP,
     *   DOEOT, OUTFIL, IERR)
C-----------------------------------------------------------------------
C   Start up routine
C   Inputs:
C      PRGM     C*6   Task name
C   Outputs:
C      INTAPE   I     Input tape number
C      NSKIP    I     Number of files to skip on INTAPE
C      NCOPY    I     Number of files to copy from INTAPE
C      INFILE   C*48  Input Disk file
C      OUTTAP   I     Output tape number
C      DOEOT    L     If true the position OUTTAP at end before copy.
C      OUTFIL   C*48  Output disk file
C      IERR     I     Return code 0=>OK.
C-----------------------------------------------------------------------
      CHARACTER PRGM*6, INFILE*48, OUTFIL*48
      INTEGER   INTAPE, NSKIP, NCOPY, OUTTAP, IERR
      LOGICAL   DOEOT
C
      HOLLERITH XIFIL(12), XOFIL(12)
      INTEGER   NPARMS, ISCR(512), IROUND
      REAL      XINTA, XNSKIP, XNCOPY, XOUTT, XDOEOT
      COMMON /INPRMS/ XINTA, XNSKIP, XNCOPY, XIFIL, XOUTT, XDOEOT, XOFIL
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DFIL.INC'
C-----------------------------------------------------------------------
C                                       Init
      IERR = 9
      CALL ZDCHIN (.TRUE., ISCR)
      NPARMS = 29
      CALL GTPARM (PRGM, NPARMS, RQUICK, XINTA, ISCR, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Tapes allowed?
      IF ((NPOPS.GT.NINTRN) .OR. (ISBTCH.EQ.32000)) THEN
         WRITE (MSGTXT,1010)
         GO TO 970
         END IF
C                                       Convert inputs
      INTAPE = IROUND (XINTA)
      NSKIP = IROUND (XNSKIP)
      NCOPY = IROUND (XNCOPY)
      OUTTAP = IROUND (XOUTT)
      DOEOT = XDOEOT.GT.0.0
      CALL H2CHR (48, 1, XIFIL, INFILE)
      CALL H2CHR (48, 1, XOFIL, OUTFIL)
C                                       Defaults and check
      IF ((INTAPE.EQ.0) .OR. (NTAPED.EQ.1)) INTAPE = 1
      IF ((INTAPE.LE.0) .OR. (INTAPE.GT.NTAPED)) THEN
         WRITE (MSGTXT,1020) INTAPE
         GO TO 970
         END IF
      IF ((OUTTAP.EQ.0) .OR. (NTAPED.EQ.1)) OUTTAP = 2
      IF ((OUTTAP.LE.0) .OR. (OUTTAP.GT.NTAPED)) THEN
         WRITE (MSGTXT,1020) OUTTAP
         GO TO 970
         END IF
      IF (NCOPY.LE.0) NCOPY = 1000000000
C                                       Resume AIPS
      IERR = 0
      IF (RQUICK) CALL RELPOP (0, ISCR, IERR)
      GO TO 999
C                                       User errors
 970  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1010 FORMAT ('TAPES ARE NOT ALLOWED IN BATCH JOBS')
 1020 FORMAT ('ILLEGAL TAPE DRIVE NUMBER',I7)
      END
      SUBROUTINE FCOPY (TLUNI, TLUNO, TINDI, TINDO, BCOUNT, LCNT, IERR)
C-----------------------------------------------------------------------
C   Copy a file from an open input tape to an open output tape EOF (EOI)
C   written on output before return.
C   Inputs:
C      TLUNI   I      LUN of input tape, >30 => tape else FITS disk
C      TLUNO   I      LUN of output tape, >30 => tape else FITS disk
C      TINDI   I      FTAB pointer of input
C      TINDO   I      FTAB pointer of output
C   Output:
C      LCNT    I      Number of files written
C      BCOUNT  D(*)   Byte count copied files 1-LCNT
C      IERR    I      Return code 0=>OK, -1=> end of tape, else failed
C-----------------------------------------------------------------------
      INTEGER   TLUNI, TLUNO, TINDI, TINDO, LCNT, IERR
      DOUBLE PRECISION BCOUNT(*)
C
      INTEGER   BUFSZ
      PARAMETER (BUFSZ=8191)
      INTEGER   BUFF(BUFSZ), RBYTES, GBYTES, COUNT, JERR, NBLOCK, I, IP
      CHARACTER SIMPLE*30, TEST*30
      LOGICAL   DATA, FFITS, LFITS
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      SIMPLE = 'SIMPLE'
      SIMPLE(9:9) = '='
      SIMPLE(30:30) = 'T'
      DATA = .FALSE.
      FFITS = .FALSE.
      BCOUNT(1) = 0.0D0
      LCNT = 1
C                                       Disk record pointers
      FTAB(TINDI+5) = 0
      FTAB(TINDO+5) = 0
C                                       Read input record
 100  FTAB(TINDI+5) = FTAB(TINDI+5) + 1
      RBYTES = (BUFSZ * NBITWD) / 8
C                                       Disk FITS?
      IF (TLUNI.LT.30) RBYTES = 2880
      CALL ZTPMIO ('READ', TLUNI, TINDI, RBYTES, BUFF, 1, IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR, 'INPUT'
         CALL MSGWRT (8)
         GO TO 500
         END IF
C                                       Synchronous but wait anyway
      MSGSUP = 32000
      CALL ZTPWAT (TLUNI, TINDI, 1, GBYTES, IERR)
      MSGSUP = 0
C                                       Check for EOI - EOF before any
C                                       data
      IF (((.NOT.DATA) .AND. (IERR.EQ.4)) .OR. (IERR.EQ.6)) THEN
         IERR = -1
         IF (.NOT.DATA) GO TO 999
         GO TO 500
         END IF
C                                       Trap EOF
      IF (IERR.EQ.4) THEN
         IERR = 0
         GO TO 500
         END IF
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT,1100) IERR, 'INPUT'
         CALL MSGWRT (8)
         GO TO 500
         END IF
C                                       Check for buffer overrun
      IF (GBYTES.GT.RBYTES) THEN
         MSGTXT = 'BUFFER OVERRUN - INPUT BLOCKSIZE TOO LARGE'
         IERR = 1
         GO TO 500
         END IF
C                                       FITS (logical) file at its
C                                       start?
      LFITS = MOD (GBYTES, 2880) .EQ. 0
      IF (LFITS) THEN
         CALL ZC8CL (30, 1, BUFF, TEST)
         LFITS = SIMPLE.EQ.TEST
C                                       insert an EOF
         IF ((DATA) .AND. (LFITS) .AND. (FFITS)) THEN
            IF (TLUNO.GT.30) THEN
               MSGTXT = 'Warning: EOF inserted between FITS data files'
               CALL MSGWRT (6)
               LCNT = LCNT + 1
               CALL ZTAPE ('WEOF', TLUNO, TINDO, COUNT, JERR)
               IF (JERR.NE.0) THEN
                  WRITE (MSGTXT,1100) JERR, 'WRITE EOF'
                  IERR = JERR
                  GO TO 990
                  END IF
            ELSE
               MSGTXT = 'FITS FILES APPEAR TO BE RUN TOGETHER'
               CALL MSGWRT (7)
               MSGTXT = 'CANNOT CORRECT THIS WHEN WRITING TO DISK'
               CALL MSGWRT (7)
               END IF
            END IF
         IF (.NOT.DATA) FFITS = LFITS
         END IF
C                                       Have data to write
      DATA = .TRUE.
C                                       Write record - may take several
C                                       writes
      NBLOCK = 1
C                                       Output FITS disk?
      IF (TLUNO.LT.30) THEN
         NBLOCK = GBYTES / 2880
         IF (MOD(GBYTES, 2880).NE.0) THEN
            WRITE (MSGTXT,1110) GBYTES
            IERR = 8
            GO TO 990
            END IF
         END IF
      NBLOCK = MAX (1, NBLOCK)
      RBYTES = GBYTES / NBLOCK
      DO 200 I = 1,NBLOCK
         FTAB(TINDO+5) = FTAB(TINDO+5) + 1
         IP = (I-1) * (RBYTES / 4) + 1
         BCOUNT(LCNT) = BCOUNT(LCNT) + RBYTES
         CALL ZTPMIO ('WRIT', TLUNO, TINDO, RBYTES, BUFF(IP), 1, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'OUTPUT'
            CALL MSGWRT (8)
            GO TO 500
            END IF
C                                       Synchronous but wait anyway
         CALL ZTPWAT (TLUNO, TINDO, 1, GBYTES, IERR)
         IF (IERR.NE.0) THEN
            WRITE (MSGTXT,1100) IERR, 'OUTPUT'
            GO TO 500
            END IF
 200     CONTINUE
C                                       Loop until EOF
      GO TO 100
C                                       Write output EOF
 500  IF ((DATA) .AND. (TLUNO.GT.30)) THEN
         COUNT = 1
         CALL ZTAPE ('WEOF', TLUNO, TINDO, COUNT, JERR)
         IF (JERR.NE.0) THEN
            WRITE (MSGTXT,1100) JERR, 'WRITE EOF'
            IERR = JERR
            GO TO 990
            END IF
         END IF
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1100 FORMAT ('TAPE I/O ERROR ',I3,' ON ',A)
 1110 FORMAT ('DISK OUTPUT VALID ONLY FOR FITS FORMAT, NOT RECORDS OF',
     *   I6,' BYTES')
      END
