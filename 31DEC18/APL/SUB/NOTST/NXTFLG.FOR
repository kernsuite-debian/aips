      SUBROUTINE NXTFLG (TIME, TABLE, IERR)
C-----------------------------------------------------------------------
C! Manages flagging info in tables in common.
C# EXT-appl Calibration UV
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1997-1998, 2000, 2007, 2011, 2015
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
C   Updates flagging tables in common fron an FG table.
C   Inputs:
C      TIME         R    Current time (days) for flag entries
C      TABLE        L    If table true then ignore baseline dependent
C                        and channel dependent flags
C   Inputs from common /CFMINF/(INCLUDEs C/DSEL.INC):
C      NUMFLG       I    number of current FLAG entries.
C      FGKOLS(MAXFGC) I    The column pointer array in order, SOURCE,
C                        SUBARRAY, FREQID, ANTS, TIMERANG, IFS, CHANS,
C                        PFLAGS, REASON
C      FGNUMV(MAXFGC) I    Element count for each column
C      IFGRNO       I    Current FLAG file record.
C   Output to common /CFMINF/:
C      NUMFLG     I    Number of flagging entries.
C      TMFLST     R    Time of last visibility for which flagging
C                      was checked.
C      FLGSOU(*)  I    Source id numbers to flag, 0=all.
C      FLGANT(*)  I    Antenna numbers to flag, 0=all.
C      FLGBAS(*)  I    Baseline (A1*32768+A2) numbers to flag, 0=all.
C      FLGSUB(*)  I    Subarray numbers to flag, 0=all.
C      FLGFQD(*)  I    Freqid numbers to flag, <=0=all.
C                      Following should have defaults filled in.
C      FLGBIF(*)  I    First IF to flag.
C      FLGEIF(*)  I    Highest IF to flag.
C      FLGBCH(*)  I    First channel to flag.
C      FLGECH(*)  I    Highest channel to flag.
C      FLGPOL(4,*)L    Flags for the polarizations, should correspond
C                      to selected polarization types.
C      FLGTST(*)  R    Start time of flag.
C      FLGTND(*)  R    End time of flag.
C   Output:
C      IERR       I    Return code, 0=OK, else TABIO error number.
C                         10 => too many flags
C-----------------------------------------------------------------------
      REAL      TIME
      LOGICAL   TABLE
      INTEGER   IERR
C
      INTEGER   J, NDROP, LIMIT, RECI(30), MXFLG, SOUKOL, SUBKOL,
     *   FRQKOL, ANTKOL, TIMKOL, IFKOL, CHKOL, POLKOL, REAKOL, A1, A2,
     *   IT, I4, NFGREC, I, LIMIT4
      REAL      RECORD(31)
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:PFLG.INC'
      INCLUDE 'INCS:DFLG.INC'
      EQUIVALENCE (RECORD, RECI)
      EQUIVALENCE (FGKOLS(1), SOUKOL), (FGKOLS(2), SUBKOL),
     *   (FGKOLS(3), FRQKOL), (FGKOLS(4), ANTKOL), (FGKOLS(5),TIMKOL),
     *   (FGKOLS(6), IFKOL),  (FGKOLS(7), CHKOL), (FGKOLS(8), POLKOL),
     *   (FGKOLS(9), REAKOL)
      DATA I4 /4/
C-----------------------------------------------------------------------
      IERR = 0
      MXFLG = MAXFLG
      TMFLST = TIME
C                                       Check if any flags expired.
 10   NDROP = 0
C                                       Find highest number expired flag
      IF ((NUMFLG.GT.0) .AND. (TIMORD)) THEN
         DO 20 I = 1,NUMFLG
            IF (FLGTND(I).LT.TIME) NDROP = I
 20         CONTINUE
         END IF
C                                       Compress, dropping flag.
      IF (NDROP.GT.0) THEN
         IF (NDROP.LT.NUMFLG) THEN
            LIMIT = NDROP + 1
            DO 150 I = LIMIT,NUMFLG
               IT = I - 1
               FLGTST(IT) = FLGTST(I)
               FLGTND(IT) = FLGTND(I)
               FLGSOU(IT) = FLGSOU(I)
               FLGANT(IT) = FLGANT(I)
               FLGFQD(IT) = FLGFQD(I)
               FLGBAS(IT) = FLGBAS(I)
               FLGSUB(IT) = FLGSUB(I)
               FLGBIF(IT) = FLGBIF(I)
               FLGEIF(IT) = FLGEIF(I)
               FLGBCH(IT) = FLGBCH(I)
               FLGECH(IT) = FLGECH(I)
               FLGPOL(1,IT) = FLGPOL(1,I)
               FLGPOL(2,IT) = FLGPOL(2,I)
               FLGPOL(3,IT) = FLGPOL(3,I)
               FLGPOL(4,IT) = FLGPOL(4,I)
 150           CONTINUE
            END IF
         NUMFLG = NUMFLG - 1
         GO TO 10
         END IF
C                                       Find next valid flag.
 300  NFGREC = FGBUFF(5)
C                                       Check if list exhausted
      IF (IFGRNO.GT.NFGREC) GO TO 999
C                                       Loop through records
 310  LIMIT4 = IFGRNO
      DO 360 I = LIMIT4,NFGREC
         IFGRNO = I
         IERR = 1
C                                       Read record.
         CALL TABIO ('READ', 0, IFGRNO, RECORD, FGBUFF, IERR)
C                                       Check if flagged
         IF (IERR.LT.0) GO TO 360
C                                       Check error
         IF (IERR.GT.0) GO TO 999
C                                       Check time.
         IF (TIMORD) THEN
            IF (TIME.LT.RECORD(TIMKOL)) GO TO 999
            IF (TIME.GT.RECORD(TIMKOL+1)) GO TO 360
            END IF
C                                       Check FQ ID.
         IF (RECI(FRQKOL).GT.0) THEN
            IF ((RECI(FRQKOL).NE.FRQSEL) .AND. (FRQSEL.GT.0) .AND.
     *         (RECI(FRQKOL).GT.0)) GO TO 360
            END IF
C                                       Check that starting IF
C                                       is in range
         IF ((RECI(IFKOL).GT.0).AND.
     *      (RECI(IFKOL).GT.CATUV(KINAX+KLOCIF))) GO TO 360
C                                       Check that starting
C                                       channel is in range
         IF ((RECI(CHKOL).GT.0).AND.
     *      (RECI(CHKOL).GT.CATUV(KINAX+KLOCFY))) GO TO 360
C                                       Does source number matter?
         IF ((RECI(SOUKOL).LE.0) .OR. (NSOUWD.LE.0)) GO TO 500
C                                       Search source lists
            DO 340 J = 1,NSOUWD
               IF (RECI(SOUKOL).EQ.SOUWAN(J)) GO TO 350
 340           CONTINUE
C                                       No match: is desirable?
            IF (.NOT.DOSWNT) GO TO 500
            GO TO 360
C                                       Match: is that desirable
 350        IF (DOSWNT) GO TO 500
 360     CONTINUE
C                                       No flags - bail out.
      IERR = 0
      GO TO 999
C                                       Next entry
 500  NUMFLG = NUMFLG + 1
C                                       Check if too big
      IERR = 0
      IF (NUMFLG.GT.MXFLG) THEN
         IERR = 10
         WRITE (MSGTXT,1500) MXFLG
         CALL MSGWRT (8)
         IF (TIMORD) THEN
            MSGTXT = 'USE UVCOP TO APPLY THE FG TABLE'
         ELSE
            MSGTXT = 'SORT TO TIME ORDER TO APPLY THE FG TABLE'
            END IF
         GO TO 990
         END IF
C                                       Fill in tables
      FLGTST(NUMFLG) = RECORD(TIMKOL)
      FLGTND(NUMFLG) = RECORD(TIMKOL+1)
      FLGSOU(NUMFLG) = RECI(SOUKOL)
      FLGFQD(NUMFLG) = RECI(FRQKOL)
      A1 = MIN (RECI(ANTKOL), RECI(ANTKOL+1))
      A2 = MAX (RECI(ANTKOL), RECI(ANTKOL+1))
      IF (A1.LE.0) THEN
         FLGANT(NUMFLG) = A2
         FLGBAS(NUMFLG) = 0
      ELSE
         FLGANT(NUMFLG) = RECI(ANTKOL)
         FLGBAS(NUMFLG) = A1*32768 + A2
         END IF
      FLGSUB(NUMFLG) = RECI(SUBKOL)
      FLGBIF(NUMFLG) = RECI(IFKOL)
      FLGEIF(NUMFLG) = RECI(IFKOL+1)
      IF (FLGBIF(NUMFLG).LE.0) FLGBIF(NUMFLG) = 1
      IF (FLGEIF(NUMFLG).LE.0) THEN
         IF (KLOCIF.GT.0) FLGEIF(NUMFLG) = CATUV (KINAX+KLOCIF)
         IF (KLOCIF.LE.0) FLGEIF(NUMFLG) = 1
         END IF
      FLGBCH(NUMFLG) = RECI(CHKOL)
      FLGECH(NUMFLG) = MIN (CATUV(KINAX+KLOCFY), RECI(CHKOL+1))
      IF (FLGBCH(NUMFLG).LE.0) FLGBCH(NUMFLG) = 1
      IF (FLGECH(NUMFLG).LE.0) FLGECH(NUMFLG) = CATUV (KINAX+KLOCFY)
C                                       Ensure that IF and channel
C                                       selection are in range
      FLGEIF(NUMFLG) = MIN (FLGEIF(NUMFLG), CATUV(KINAX+KLOCIF))
      FLGECH(NUMFLG) = MIN (FLGECH(NUMFLG), CATUV(KINAX+KLOCFY))
C
      CALL LG2BIT (I4, FLGPOL(1,NUMFLG), RECI(POLKOL), -1)
C                                       table ignores baseline based
C                                       and channel based
      IF (TABLE) THEN
         IF ((FLGBAS(NUMFLG).NE.0) .OR. (FLGBCH(NUMFLG).NE.1) .OR.
     *      (FLGECH(NUMFLG).NE.CATUV(KINAX+KLOCFY))) NUMFLG = NUMFLG - 1
         END IF
C                                       Increment flag counter
      IFGRNO = IFGRNO + 1
C                                       Loop back for next
      IF (IFGRNO.LE.NFGREC) GO TO 310
      GO TO 999
C                                       Error
 990  CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1500 FORMAT ('TOO MANY FLAGS AT SAME TIME (>',I7,')')
      END
