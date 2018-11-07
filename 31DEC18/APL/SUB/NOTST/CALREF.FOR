      SUBROUTINE CALREF (OLDREF, NEWREF, SUB, ROWIDX, IFFREQ, SMOTIM,
     *   MAXTIM, TBLBUF, WRKTIM, WORK1, WORK2, WORK3, WORK4, IRET)
C-----------------------------------------------------------------------
C! Adjusts the reference antenna in an SN table.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995-1999, 2003, 2011
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
C   Change the reference antenna for a single IF and polarization in an
C   SN table from antenna number OLDREF to antenna number NEWREF in
C   subarray number SUB, interpolating between or extrapolating from
C   tabulated values of the phase, delay, and rate corrections for
C   OLDREF relative to NEWREF that have been extracted from the table
C   and smoothed using boxcar functions with widths given by SMOTIM. Use
C   the smoothed rate corrections to provide time derivatives for the
C   phase and delay corrections during interpolation/extrapolation.
C
C   The SN table may be assumed to be open and in time order.
C   Inputs:
C      OLDREF  I          Old reference antenna number
C      NEWREF  I          New reference antenna number
C      SUB     I          Subarray number
C      ROWIDX  I(9)       Indices into an SN table row for
C                          1 - antenna number
C                          2 - reference antenna for current IF and
C                              polarization
C                          3 - subarray number
C                          4 - weight for corrections for current IF and
C                              polarization
C                          5 - time
C                          6 - real gain correction for current IF and
C                              polarization
C                          7 - imaginary gain correction for current IF
C                              and polarization
C                          8 - delay correction for current IF and
C                              polarization
C                          9 - rate correction for current IF and
C                              polarization
C                          All assumed to be valid indices into an SN
C                          table row buffer.
C      IFFREQ  D           IF reference frequency in Hz (i.e. data
C                           reference frequency plus IF offset)
C      SMOTIM  R(3)        Boxcar smoothing times for phase (1),
C                           delay (2), and rate (3) in days. Zero or
C                           negative values imply no smoothing before
C                           interpolation
C      MAXTIM  I           Maximum number of time slots in interpolation
C                          workspace. Assumed to be positive.
C   Input/Output:
C      TBLBUF  I(*)        Table I/O buffer for SN table. Assumed to
C                           have been initialized by TABOPN. Updated
C                           on exit
C      WRKTIM  R(MAXTIM)   Interpolation workspace. Trashed on exit.
C      WORK1   R(MAXTIM)   Interpolation workspace. Trashed on exit.
C      WORK2   R(MAXTIM)   Interpolation workspace. Trashed on exit.
C      WORK3   R(MAXTIM)   Interpolation workspace. Trashed on exit.
C      WORK4   R(MAXTIM)   Interpolation workspace. Trashed on exit.
C   Output:
C      IRET    I           Return status
C                            0 - table updated
C                            1 - can not reference OLDREF to NEWREF;
C                                table is unchanged and an error
C                                message has been issued
C                            2 - insufficient interpolation workspace;
C                                table is unchanged and an error
C                                message has been issued
C                            3 - error detected while filling workspace;
C                                table is unchanged and an error
C                                message has been issued
C                            4 - error detected while updating table;
C                                table may be partly updated and an
C                                error message has been issued
C-----------------------------------------------------------------------
      INTEGER   OLDREF, NEWREF, SUB, ROWIDX(9)
      DOUBLE PRECISION IFFREQ
      REAL      SMOTIM(3)
      INTEGER   MAXTIM, TBLBUF(*)
      REAL      WRKTIM(MAXTIM), WORK1(MAXTIM), WORK2(MAXTIM)
      REAL      WORK3(MAXTIM), WORK4(MAXTIM)
      INTEGER   IRET
C
C     Local variables
C
C     NUMROW    Number of rows in SN table
C     NUMTIM    Number of tabulated phase, delay, and rate corrections
C                for OLDREF relative to NEWREF
C     SMALL     Small number threshold
C     ROW       TABIO code for row I/O
C     SECPDY    Number of seconds in a day
C     FTLERR    Message priority for fatal error messages
C
C     TIMIDX    Time index in SN table row
C     ANTIDX    Antenna number index in SN table row
C     SUBIDX    Subarray number index in SN table row
C     REFIDX    Reference antenna number index in SN table row
C     GRIDX     Real gain index in SN table row
C     GIIDX     Imaginary gain index in SN table row
C     DELIDX    Delay index in SN table row
C     RATIDX    Rate index in SN table row
C     WTIDX     Weight index in SN table row
C
C     SNROW     SN table row number
C     SNROWR    SN row buffer as array of REAL
C     SNROWI    SN row buffer as array of INTEGER
C     SNROWD    SN row buffer as array of DOUBLE PRECISION
C
C     BEFORE    Index of latest time before current record in
C               interpolation table or 1 if there are no times
C               prior to the current record in the interpolation
C               table
C     AFTER     Index of the earliest time after the current record
C               in the interpolation or NUMTIM if there are no
C               times after the current record in the interpolation
C               table
C
C     WT1       Weight of interpolation point 1
C     WT2       Weight of interpolation point 2
C     Y1        Value at interpolation point 1
C     Y2        Value at interpolation point 2
C
C     GR        Real part of gain
C     GI        Imaginary part of gain
C
C     PHASE     Interpolated phase
C
      INTEGER   NUMROW
      INTEGER   NUMTIM
      REAL      SMALL
      INTEGER   ROW
      PARAMETER (ROW = 0)
      DOUBLE PRECISION SECPDY
      PARAMETER (SECPDY = 24.0D0 * 60.0D0 * 60.0D0)
      INTEGER   FTLERR
      PARAMETER (FTLERR = 8)
C
      INTEGER   TIMIDX, ANTIDX, SUBIDX, REFIDX, GRIDX, GIIDX, DELIDX
      INTEGER   RATIDX, WTIDX
C
      INTEGER   SNROW
      REAL      SNROWR(1024)
      INTEGER   SNROWI(1024)
      DOUBLE PRECISION SNROWD(512)
      EQUIVALENCE (SNROWR, SNROWI, SNROWD)
C
      INTEGER   BEFORE, AFTER, TABC, SORC, NUMOLD
      REAL      WT1, WT2, Y1, Y2, GR, GI, PHASE
      CHARACTER TABIOC(2)*4
      LOGICAL   WASPHS
C
C     REAL      SLAMCH
C     EXTERNAL  SLAMCH
C
      INCLUDE 'INCS:PSTD.INC'
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA TABIOC /'FLAG','WRIT'/
C-----------------------------------------------------------------------
      SORC = -1
C                                       Number of rows in table
      NUMROW = TBLBUF(5)
      WASPHS = .FALSE.
C                                       No change is needed.
      IF ((NUMROW.EQ.0) .OR. (NEWREF.EQ.OLDREF)) THEN
         IRET = 0
C                                       Update the table:
      ELSE
         NUMOLD = 0
C
C        Update the table rows with refence antenna OLDREF in subarray
C        SUB using phase, delay, and rate information for OLDREF
C        relative to NEWREF extracted from the table and set IRET to
C        zero otherwise issue a fatal error message and set IRET to
C        a positive number:
C                                       Threshold value small numbers
C        SMALL = SQRT (SLAMCH ('safe minimum'))
         SMALL = 1.E-10
C                                       Mnemonic pointers into table
         ANTIDX = ROWIDX(1)
         REFIDX = ROWIDX(2)
         SUBIDX = ROWIDX(3)
         WTIDX  = ROWIDX(4)
         TIMIDX = ROWIDX(5)
         GRIDX  = ROWIDX(6)
         GIIDX  = ROWIDX(7)
         DELIDX = ROWIDX(8)
         RATIDX = ROWIDX(9)
C
C        Find unflagged SN table rows that directly relate OLDREF to
C        NEWREF in subarray SUB and the have unblanked values for the
C        gain, delay and rate corrections with a positive weight and
C        non-negligable gain amplitude. Store the number of such
C        records as NUMTIM, their times in WRKTIM and the phase, rate
C        and delay corrections for OLDREF relative to NEWREF in WORK2,
C        WORK3, and WORK4. Issue a fatal error message and set IRET
C        to 3 if there is an error reading the table; issue a fatal
C        error message and set IRET to 2 if NUMTIM would exceed MAXTIM;
C        otherwise set IRET to 0.
C
         IRET  = 0
         SNROW = 0
         NUMTIM = 0
 10      IF ((SNROW.NE.NUMROW) .AND. (IRET.EQ.0)) THEN
            SNROW = SNROW + 1
            CALL TABIO ('READ', ROW, SNROW, SNROWR, TBLBUF, IRET)
C                                       Ignore flagged row:
            IF (IRET.LT.0) THEN
               IRET = 0
            ELSE IF (IRET.EQ.0) THEN
               IF (ABS(SNROWR(GIIDX)).GT.SMALL) WASPHS = .TRUE.
               IF (((SNROWI(SUBIDX).EQ.SUB) .OR. (SNROWI(SUBIDX).LE.0))
     *             .AND. (SNROWI(REFIDX).EQ.OLDREF) .AND.
     *             (SNROWR(WTIDX).GT.0.0)) NUMOLD = NUMOLD + 1
               IF (((SNROWI(SUBIDX).EQ.SUB) .OR. (SNROWI(SUBIDX).LE.0))
     *             .AND. (((SNROWI(ANTIDX).EQ.OLDREF) .AND.
     *             (SNROWI(REFIDX).EQ.NEWREF)) .OR.
     *             ((SNROWI(REFIDX).EQ.OLDREF) .AND.
     *             (SNROWI(ANTIDX).EQ.NEWREF)))
     *             .AND. (SNROWR(WTIDX).GT.0.0)) THEN
C
C                 The current row relates OLDREF to NEWREF with positive
C                 weight. If the gain, delay and rate corrections are
C                 not blanked and the gain correction has a non-
C                 negligable amplitude then store store the phase,
C                 delay, and rate corrections for OLDREF relative to
C                 NEWREF if NUMTIM is less than MAXTIM or issue a fatal
C                 error message and set IRET to 2 if NUMTIM equals
C                 MAXTIM:
C
                  IF ((SNROWR(GRIDX).NE.FBLANK)
     *                .AND. (SNROWR(GIIDX).NE.FBLANK)
     *                .AND. (SNROWR(DELIDX).NE.FBLANK)
     *                .AND. (SNROWR(RATIDX).NE.FBLANK)) THEN
C
C                    If the gain amplitude is non-negligable then store
C                    the phase, delay and rate corrections for OLDREF
C                    relative to NEWREF if NUMTIM is less than MAXTIM
C                    or issue a fatal error message and set IRET to 2
C                    if NUMTIM equals MAXTIM:
C
                     IF ((ABS (SNROWR(GRIDX)).GE.SMALL)
     *                   .OR. (ABS (SNROWR(GIIDX)).GE.SMALL)) THEN
C
C                       Store the phase, delay and rate corrections for
C                       OLDREF relative to NEWREF if NUMTIM is less than
C                       MAXTIM or issue a fatal error message and set
C                       IRET to 2 if NUMTIM equals MAXTIM:
C
                        IF (NUMTIM.NE.MAXTIM) THEN
C
C                          Store the phase, delay and rate corrections
C                          for OLDREF relative to NEWREF and increment
C                          NUMTIM:
C
                           NUMTIM = NUMTIM + 1
                           WRKTIM(NUMTIM) = REAL (SNROWD(TIMIDX))
C
C                             Record gives corrections for OLDREF
C                             relative to NEWREF.
                           IF (SNROWI(ANTIDX).EQ.OLDREF) THEN
C                                       +- 90 degrees
                              IF (ABS (SNROWR(GRIDX)).LT.SMALL) THEN
                                 IF (SNROWR(GIIDX).GT.0.0) THEN
                                    WORK2(NUMTIM) = REAL (PI / 2.0D0)
                                 ELSE
                                    WORK2(NUMTIM) = -REAL (PI / 2.0D0)
                                    END IF
                              ELSE
                                 WORK2(NUMTIM) = ATAN2 (SNROWR(GIIDX),
     *                                                  SNROWR(GRIDX))
                                 END IF
                              WORK3(NUMTIM) = SNROWR(DELIDX)
                              WORK4(NUMTIM) = SNROWR(RATIDX)
C
C                             Record gives corrections for NEWREF
C                             relative to OLDREF. Flip signs to get
C                             corrections for OLDREF realtive to
C                             NEWREF:
                           ELSE
C                                       +- 90 degrees
                              IF (ABS (SNROWR(GRIDX)).LT.SMALL) THEN
                                 IF (SNROWR(GIIDX).GT.0.0) THEN
                                    WORK2(NUMTIM) = -REAL (PI / 2.0D0)
                                 ELSE
                                    WORK2(NUMTIM) = REAL (PI / 2.0D0)
                                    END IF
                              ELSE
                                 WORK2(NUMTIM) = -ATAN2 (SNROWR(GIIDX),
     *                                                   SNROWR(GRIDX))
                                 END IF
                              WORK3(NUMTIM) = -SNROWR(DELIDX)
                              WORK4(NUMTIM) = -SNROWR(RATIDX)
                              END IF
C
C                          No room left in interpolation table. Issue
C                          a fatal error message and set IRET to 2
                        ELSE
                           WRITE (MSGTXT,9010) MAXTIM
                           CALL MSGWRT (FTLERR)
                           IRET = 2
                           END IF
                        END IF
                     END IF
                  END IF
C                                       Read error. Issue a fatal error
C                                       message and set IRET to 3:
            ELSE
               WRITE (MSGTXT, 9011) IRET
               CALL MSGWRT (FTLERR)
               IRET = 2
               END IF
C                                       loop for more
            GO TO 10
            END IF
C
C        If no error conditions were detected while reading the
C        interpolation data then update the table using this data
C        or issue a fatal error message and set IRET to 1 if no
C        data were accumulated.  Issue a fatal error message and
C        set IRET to 4 if an error is detected while updating the
C        table.
C
         IF (IRET.EQ.0) THEN
C                                       No data relates OLDREF to
C                                       NEWREF. Issue a fatal error.
C
            IF (NUMTIM.EQ.0) THEN
               WRITE (MSGTXT,9012) OLDREF, NEWREF, SUB
               IF ((WASPHS) .AND. (NUMOLD.GT.0)) CALL MSGWRT (FTLERR)
               IF (NUMOLD.GT.0) IRET = 1
C                                       Smooth phases to WORK1, delays
C                                       to WORK2, and rates to WORK3:
            ELSE
               IF (SMOTIM(1).GE.1.1E-6) THEN
                  CALL BOXBSM (SMOTIM(1), WRKTIM, WORK2, SORC, FBLANK,
     *               NUMTIM, WORK1)
               ELSE
                  CALL RCOPY (NUMTIM, WORK2, WORK1)
                  END IF
               IF (SMOTIM(2).GE.1.1E-6) THEN
                  CALL BOXBSM (SMOTIM(2), WRKTIM, WORK3, SORC, FBLANK,
     *               NUMTIM, WORK2)
               ELSE
                  CALL RCOPY (NUMTIM, WORK3, WORK2)
                  END IF
               IF (SMOTIM(3).GE.1.1E-6) THEN
                  CALL BOXBSM (SMOTIM(3), WRKTIM, WORK4, SORC, FBLANK,
     *               NUMTIM, WORK3)
               ELSE
                  CALL RCOPY (NUMTIM, WORK4, WORK3)
                  END IF
C
C              Update reference antenna in SN table using data
C              interpolated or extrapolated from WORK1, WORK2 and WORK3.
C              Issue a fatal error message and set IRET to 4 if a read
C              or write error is detected.
C
C                                       Initialize pointers
               BEFORE = 1
               AFTER  = MIN (2, NUMTIM)
C                                       Update table rows with reference
C                                       antenna OLDREF in subarray SUB:
               SNROW = 0
 20            IF ((SNROW.NE.NUMROW) .AND. (IRET.EQ.0)) THEN
                  SNROW = SNROW + 1
                  CALL TABIO ('READ', ROW, SNROW, SNROWR, TBLBUF, IRET)
C                                       Flagged rows should be adjusted
C                                       as well as unflagged rows.
                  IF (IRET.LE.0) THEN
                     TABC = MAX (1, IRET+2)
                     IF ((SNROWI(REFIDX).EQ.OLDREF) .AND.
     *                  ((SNROWI(SUBIDX).EQ.SUB) .OR.
     *                  (SNROWI(SUBIDX).EQ.0))) THEN
C
C                       Increment the interpolation table pointers
C                       until AFTER is either equal to NUMTIM or
C                       points to a time equal to or later than the
C                       SN row time:
C
 30                     IF ((AFTER.NE.NUMTIM) .AND. (REAL
     *                     (SNROWD(TIMIDX)).GT.WRKTIM(AFTER))) THEN
                           BEFORE = BEFORE + 1
                           AFTER = AFTER + 1
                           GO TO 30
                           END IF
C
C                       There are 4 cases to consider
C                       1 - If WRKTIM(AFTER) < SNROWD(TIMIDX) then
C                           the row time is after the last entry in
C                           the table and the corrections must be
C                           extrapolated.
C                       2 - If WRKTIM(BEFORE) > SNROWD(TIMIDX) then
C                           the row time is before the first entry in
C                           the table and the corrections must be
C                           extrapolated
C                       3 - If WRKTIM(BEFORE) = SNROWD(TIMIDX)
C                           = WRKTIM(AFTER) then there may be two
C                           tabulated corrections for the row and
C                           they may be averaged. Interpolation
C                           would cause a divide by zero fault.
C                       4 - Otherwise the table entries may be
C                           interpolated.
C
C                       Set interpolation weights:
C
                        IF (WRKTIM(AFTER)
     *                     .LT.REAL (SNROWD(TIMIDX))) THEN
                           WT1 = 1.0
                           WT2 = 0.0
                        ELSE IF (WRKTIM(BEFORE)
     *                          .GT.REAL (SNROWD(TIMIDX))) THEN
                           WT1 = 0.0
                           WT2 = 1.0
                        ELSE IF ((WRKTIM(BEFORE)
     *                           .EQ.REAL (SNROWD(TIMIDX)))
     *                           .AND. (WRKTIM(BEFORE)
     *                                 .EQ.WRKTIM(AFTER))) THEN
                           WT1 = 0.5
                           WT2 = 0.5
                        ELSE
                           WT1 = (WRKTIM(AFTER)
     *                            - REAL (SNROWD(TIMIDX)))
     *                           / (WRKTIM(AFTER) - WRKTIM(BEFORE))
                           WT2 = 1.0 - WT1
                           END IF
C
C                       Update phase correction if gain correction
C                       is not blank using the tabulated rate
C                       rate to calculate time derivatives:
C
                        IF ((SNROWR(GRIDX).NE.FBLANK)
     *                      .AND. (SNROWR(GIIDX).NE.FBLANK)) THEN
                           IF (WRKTIM(AFTER)
     *                        .LT.REAL (SNROWD(TIMIDX))) THEN
                              Y1 = WORK1(AFTER)
     *                             + TWOPI * SECPDY * WORK3(AFTER)
     *                             * (REAL (SNROWD(TIMIDX))
     *                                - WRKTIM(AFTER)) * IFFREQ
                              Y2 = 0.0
                           ELSE IF (WRKTIM(BEFORE)
     *                             .GT.REAL (SNROWD(TIMIDX))) THEN
                              Y1 = 0.0
                              Y2 = WORK1(BEFORE)
     *                             + TWOPI * SECPDY * WORK3(BEFORE)
     *                             * (REAL (SNROWD(TIMIDX))
     *                                - WRKTIM(BEFORE)) * IFFREQ
                           ELSE IF ((WRKTIM(BEFORE)
     *                              .EQ.REAL (SNROWD(TIMIDX)))
     *                              .AND. (WRKTIM(BEFORE)
     *                                    .EQ.WRKTIM(AFTER))) THEN
                              Y1 = WORK1(BEFORE)
                              Y2 = WORK1(AFTER)
                           ELSE
                              Y1 = WORK1(BEFORE)
     *                             + TWOPI * SECPDY * WORK3(BEFORE)
     *                             * (REAL (SNROWD(TIMIDX))
     *                                - WRKTIM(BEFORE)) * IFFREQ
                              Y2 = WORK1(AFTER)
     *                             + TWOPI * SECPDY * WORK3(AFTER)
     *                             * (REAL (SNROWD(TIMIDX))
     *                                - WRKTIM(AFTER)) * IFFREQ
                              END IF
                           PHASE = WT1 * Y1 + WT2 * Y2
                           GR = SNROWR(GRIDX)
                           GI = SNROWR(GIIDX)
                           SNROWR(GRIDX) = GR * COS(PHASE)
     *                                     - GI * SIN(PHASE)
                           SNROWR(GIIDX) = GR * SIN(PHASE)
     *                                     + GI * COS(PHASE)
                           END IF
C
C                       Update delay correction if it is not blank:
C
                        IF (SNROWR(DELIDX).NE.FBLANK) THEN
                           IF (WRKTIM(AFTER)
     *                        .LT.REAL (SNROWD(TIMIDX))) THEN
                              Y1 = WORK2(AFTER)
     *                             + WORK3(AFTER) * SECPDY
     *                             * (REAL (SNROWD(TIMIDX))
     *                                - WRKTIM(AFTER))
                              Y2 = 0.0
                           ELSE IF (WRKTIM(BEFORE)
     *                             .GT.REAL (SNROWD(TIMIDX))) THEN
                              Y1 = 0.0
                              Y2 = WORK2(BEFORE)
     *                             + WORK3(BEFORE) * SECPDY
     *                             * (REAL (SNROWD(TIMIDX))
     *                                - WRKTIM(BEFORE))
                           ELSE IF ((WRKTIM(BEFORE)
     *                              .EQ.REAL (SNROWD(TIMIDX)))
     *                              .AND. (WRKTIM(BEFORE)
     *                                    .EQ.WRKTIM(AFTER))) THEN
                              Y1 = WORK2(BEFORE)
                              Y2 = WORK2(AFTER)
                           ELSE
                              Y1 = WORK2(BEFORE)
     *                             + WORK3(BEFORE) * SECPDY
     *                             * (REAL (SNROWD(TIMIDX))
     *                                - WRKTIM(BEFORE))
                              Y2 = WORK2(AFTER)
     *                             + WORK3(AFTER) * SECPDY
     *                             * (REAL (SNROWD(TIMIDX))
     *                                - WRKTIM(AFTER))
                              END IF
                           SNROWR(DELIDX) = SNROWR(DELIDX)
     *                                      + WT1 * Y1 + WT2 * Y2
                           END IF
C
C                       Adjust rate correction if it is not blanked:
C
                        IF (SNROWR(RATIDX).NE.FBLANK) THEN
                           IF (WRKTIM(AFTER)
     *                        .LT.REAL (SNROWD(TIMIDX))) THEN
                              Y1 = WORK3(AFTER)
                              Y2 = 0.0
                           ELSE IF (WRKTIM(BEFORE)
     *                             .GT.REAL (SNROWD(TIMIDX))) THEN
                              Y1 = 0.0
                              Y2 = WORK3(BEFORE)
                           ELSE
                              Y1 = WORK3(BEFORE)
                              Y2 = WORK3(AFTER)
                              END IF
                           SNROWR(RATIDX) = SNROWR(RATIDX)
     *                                         + WT1 * Y1 + WT2 * Y2
                           END IF
                        SNROWI(REFIDX) = NEWREF
C                                       Write updated record to table
                        CALL TABIO (TABIOC(TABC), ROW, SNROW, SNROWR,
     *                     TBLBUF, IRET)
C                                       Write error. Set IRET to 4:
                        IF (IRET.NE.0) THEN
                           WRITE (MSGTXT,9030) IRET
                           CALL MSGWRT (FTLERR)
                           IRET = 4
                           END IF
                        END IF
C                                       Table read error. set IRET to 4
                  ELSE
                     WRITE (MSGTXT,9030) IRET
                     CALL MSGWRT (FTLERR)
                     IRET = 4
                     END IF
C                                       loop for more
                  GO TO 20
                  END IF
               END IF
            END IF
         END IF
C
 999  RETURN
C-----------------------------------------------------------------------
 9010 FORMAT ('CALREF: INTERPOLATION TABLE SPACE (', I6, ' ENTRIES) ',
     *        'EXHAUSTED')
 9011 FORMAT ('CALREF: TABIO ERROR ', I4,
     *        ' BUILDING INTERPOLATION TABLES')
 9012 FORMAT ('CALREF: CANNOT RELATE ANTENNA ', I2, ' TO ', I2,
     *        ' IN SUBARRAY ', I2)
 9030 FORMAT ('CALREF: TABIO ERROR ', I4, ' UPDATING SN TABLE')
      END

