      SUBROUTINE GETFQ (IDFREQ, DISK, CNO, CATFQ, LUN, FREQFQ, TBWFQ,
     *   CHBWFQ, SIDFQ, BNDCOD, IERR)
C-----------------------------------------------------------------------
C! Find info on a given frequency id.
C# EXT-appl UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2012
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
C   Routine to look up frequency info for a given freq. id.
C   If can't find FQ/CH table the info will be obtained from
C   the common maintained by UVPGET; UVPGET should be called before
C   calling GETFQ
C   Inputs:
C      IDFREQ   I       Freq. id
C      DISK     I       Disk number
C      CNO      I       Catalog slot number
C      CATFQ   I(256)  Catalog header
C      LUN      I       LUN to use. (e.g. 25)
C   Output:
C      FREQFQ   D(*)    Frequency for each IF (Hz)
C      TBWFQ    R(*)    Bandwidth for each IF (Hz)
C      CHBWFQ   R(*)    Ch. sep for each IF (Hz)
C      SIDFQ    I(*)    Sideband for each IF
C      BNDCOD   C*8(*)  band code for each IF
C   Output:
C      IERR     I       Return code. 0=OK, else failed.
C                          11 => Couldn't find IDFREQ in FQ table.
C-----------------------------------------------------------------------
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   IDFREQ, DISK, CNO, CATFQ(256), LUN, SIDFQ(MAXIF),
     *   IERR
      DOUBLE PRECISION FREQFQ(MAXIF)
      REAL      TBWFQ(MAXIF), CHBWFQ(MAXIF)
      CHARACTER BNDCOD(*)*(*)
C
      INTEGER   BUFFER(512), NUMIFQ, JERR, RNOFQ, KOLSFQ(MAXFQC),
     *   NUMVFQ(MAXFQC), FQNUMB, IFQN, I, FQID, IIF, CATEQU(256),
     *   MLOCF
      LOGICAL   TABLE, FQEXIS, FITASC, CHEXIS
      REAL      CATRFQ(256)
      DOUBLE PRECISION CATDFQ(128)
      HOLLERITH CATHFQ(256)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DUVH.INC'
      EQUIVALENCE (CATDFQ, CATRFQ, CATEQU, CATHFQ)
C-----------------------------------------------------------------------
      IERR = 0
      CALL COPY (256, CATFQ, CATEQU)
      CALL AXEFND (8, 'FREQ    ', CATEQU(KIDIM), CATHFQ(KHCTP), MLOCF,
     *   IERR)
      IF (IERR.NE.0) THEN
         WRITE (MSGTXT, 1020) IERR
         GO TO 990
         END IF
C                                       Does table exist
      CHEXIS = .FALSE.
      CALL ISTAB ('FQ', DISK, CNO, 1, LUN, BUFFER, TABLE, FQEXIS,
     *   FITASC, IERR)
      IF (.NOT. FQEXIS) THEN
         CALL ISTAB ('CH', DISK, CNO, 1, LUN, BUFFER, TABLE, CHEXIS,
     *      FITASC, IERR)
         END IF
      IF ((.NOT.FQEXIS) .AND. (.NOT.CHEXIS)) GO TO 500
      IF (CHEXIS) GO TO 400
C                                       Open table and fill arrays
      CALL FQINI ('READ', BUFFER, DISK, CNO, 1, CATFQ, LUN, RNOFQ,
     *   KOLSFQ, NUMVFQ, NUMIFQ, IERR)
      IF (IERR.NE.0) GO TO 999
      FQNUMB = BUFFER(5)
      IF ((IDFREQ.EQ.-1) .AND. (FQNUMB.GT.1)) THEN
         IERR = 1
         WRITE (MSGTXT,1000) IDFREQ, FQNUMB
         CALL MSGWRT (8)
         WRITE (MSGTXT,1010)
         GO TO 990
         END IF
C                                       Find match to IDFREQ
      DO 100 I = 1,FQNUMB
         IFQN = I
         CALL TABFQ ('READ', BUFFER, IFQN, KOLSFQ, NUMVFQ, NUMIFQ, FQID,
     *      FREQFQ, CHBWFQ, TBWFQ, SIDFQ, BNDCOD, IERR)
         IF (IERR.NE.0) GO TO 999
         IF ((FQID.EQ.IDFREQ) .OR. (FQNUMB.EQ.1)) THEN
            DO 50 IIF = 1, NUMIFQ
               FREQFQ(IIF) = CATDFQ(KDCRV+MLOCF) + FREQFQ(IIF)
 50            CONTINUE
            GO TO 200
            END IF
 100     CONTINUE
C                                       No match
      IERR = 11
 200  CALL TABIO ('CLOS', 0, RNOFQ, BUFFER, BUFFER, JERR)
      GO TO 999
C                                       Use CH table
 400  CALL CHNDAT ('READ', BUFFER, DISK, CNO, 1, CATFQ, LUN, NUMIFQ,
     *   FREQFQ, SIDFQ, CHBWFQ, BNDCOD, IDFREQ, IERR)
      IF (IERR.NE.0) GO TO 999
      DO 450 IIF = 1, NUMIFQ
         FREQFQ(IIF) = CATDFQ(KDCRV+MLOCF) + FREQFQ(IIF)
         TBWFQ(IIF) = ABS(CHBWFQ(IIF) * CATFQ(KINAX+MLOCF))
 450     CONTINUE
      GO TO 999
C                                       Get info from header.
 500  IERR = 0
      FREQFQ(1) = CATDFQ(KDCRV+MLOCF)
      CHBWFQ(1) = CATRFQ(KRCIC+MLOCF)
      TBWFQ(1) = ABS(CHBWFQ(1) * CATFQ(KINAX+MLOCF))
      SIDFQ(1) = 1
      GO TO 999
C
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1000 FORMAT ('GETFQ: FREQSEL = ',I3,'; # ROWS IN FQ TABLE = ',I4)
 1010 FORMAT ('GETFQ: RERUN TASK SELECTING FREQSEL EXPLICITLY')
 1020 FORMAT ('GETFQ: ERROR ',I3,' DETERMINING FREQ. AXIS')
      END

