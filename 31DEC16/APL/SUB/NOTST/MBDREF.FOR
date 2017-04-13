      SUBROUTINE MBDREF (ANT, REFAN, SUB, KOLS, BUFFER, SMOTIM, MAXTIM,
     *   WRKTIM, WORK1, WORK2, WORK3, IRET)
C-----------------------------------------------------------------------
C! Adjusts the multi-band delay for new reference antenna in an SN table
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2003, 2015
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
C   Subroutine to adjust the multi-band delay for a new reference
C   antenna in a uv data file.
C   The table is first read to find all data relating ANT and REFAN.
C   These data are then smoothed and the resulting ANT-REFAN are used
C   in a second pass through the table to adjust data using ANT as a
C   reference antenna to REFAN as the reference antenna.
C        The amplitudes are not affected.
C        Several work arrays are passed which are used for storing,
C   smoothing and interpolating data.
C        The table should already be open and BUFFER should be the
C   buffer used by TABINI (or other table opening routines).
C   Input:
C    ANT      I        Old reference antenna
C    REFAN    I        New reference antenna
C    SUB      I        Subarray desired
C    KOLS     I(7)     Array of TABIO column pointers in order:
C                         antenna, ref. antenna, subarray, weight, time,
C                         multi-band delay, dispersion
C    BUFFER   I(512)   Buffer for TABIO use; table must already be open
C    SMOTIM   R(3)     Boxcar averaging times (days) 1=phase, 2=delay
C                         3 = rate
C    MAXTIM   I        Maximum number of times (dim of WRKTIM etc)
C   Output:
C    WRKTIM   R(*)     Large work array.
C    WORK1    R(*)     Large work array, same size as WRKTIM
C    WORK2    R(*)     Large work array, same size as WRKTIM
C    WORK3    R(*)     Large work array, same size as WRKTIM
C    IRET     I        Return code 0=OK, else failed.
C-----------------------------------------------------------------------
      INTEGER   ANT, REFAN, SUB, KOLS(7), BUFFER(*), MAXTIM, IRET
      REAL      SMOTIM(3), WORK1(*), WORK2(*), WORK3(*), WRKTIM(*)
C
      INCLUDE 'INCS:PUVD.INC'
      INTEGER   NUMTIM, IRCODE, RECORD(XCLRSZ), LKOLS(7), SORC, ANTKOL,
     *   REFKOL, SUBKOL, WTKOL, TIMKOL, DELKOL, NUMREC, LOOPR, IPNT1,
     *   IPNT2, DISKOL
      REAL      RECR(XCLRSZ), TIME1, TIME2, TIME, WT1, WT2, DELCOR, DISP
      DOUBLE PRECISION RECD(XCLRSZ/2), TIMOFF
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      EQUIVALENCE (RECORD, RECR, RECD)
      EQUIVALENCE
     *   (LKOLS(1), ANTKOL), (LKOLS(2), REFKOL), (LKOLS(3), SUBKOL),
     *   (LKOLS(4), WTKOL), (LKOLS(5), TIMKOL), (LKOLS(6), DELKOL),
     *   (LKOLS(7), DISKOL)
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      IRET = 0
      SORC = -1
C                                       Get column pointers
      CALL COPY (7, KOLS, LKOLS)
C                                       Get number of records in table
      NUMREC = BUFFER(5)
      IF (NUMREC.LE.0) GO TO 999
C                                       Loop thru table referring ANT
C                                       to REFAN.
      NUMTIM = 0
      DO 100 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) GO TO 100
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         IF (RECORD(SUBKOL).NE.SUB) GO TO 100
         IF ((RECORD(ANTKOL).NE.ANT) .AND. (RECORD(ANTKOL).NE.REFAN))
     *      GO TO 100
         IF ((RECORD(REFKOL).NE.ANT) .AND. (RECORD(REFKOL).NE.REFAN))
     *      GO TO 100
         IF (RECORD(ANTKOL).EQ.RECORD(REFKOL)) GO TO 100
         IF ((RECR(WTKOL).LE.0.0) .OR. (RECR(DELKOL).EQ.FBLANK))
     *      GO TO 100
         IF (NUMTIM.GE.MAXTIM) GO TO 100
            NUMTIM = NUMTIM + 1
            IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
            WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
C                                       REFAN is reference ant
            IF (RECORD(REFKOL).NE.ANT) THEN
               WORK2(NUMTIM) = RECR(DELKOL)
               WORK3(NUMTIM) = RECR(DISKOL)
C                                       ANT is reference ant
            ELSE
               WORK2(NUMTIM) = -RECR(DELKOL)
               WORK3(NUMTIM) = -RECR(DISKOL)
               END IF
 100     CONTINUE
      IF (NUMTIM.LE.0) GO TO 999
C                                       Smooth as requested
      IF (SMOTIM(3).GT.1.1E-6) THEN
         CALL BOXBSM (SMOTIM(3), WRKTIM, WORK2, SORC, FBLANK, NUMTIM,
     *      WORK1)
         CALL BOXBSM (SMOTIM(3), WRKTIM, WORK3, SORC, FBLANK, NUMTIM,
     *      WORK2)
      ELSE
         CALL RCOPY (NUMTIM, WORK2, WORK1)
         CALL RCOPY (NUMTIM, WORK3, WORK2)
         END IF
C                                       Set up for interpolation
      IPNT1 = 1
      IPNT2 = 2
      TIME1 = WRKTIM(1)
      TIME2 = WRKTIM(2)
C                                       Loop thru table changing any
C                                       data with ref=ANT to ref=REFAN
      DO 200 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.NE.0) GO TO 900
C                                       See if wanted.
         IF ((RECORD(SUBKOL).EQ.SUB) .AND. (RECORD(REFKOL).EQ.ANT)) THEN
C                                       Don't want to change REFAN here
C                                       because CALREF will do it for
C                                       the IF dependent values.
CCC            RECORD(REFKOL) = REFAN
C                                       Interpolate
            IF ((RECR(WTKOL).GT.0.0) .AND. (RECR(DELKOL).NE.FBLANK))
     *         THEN
               TIME = RECD(TIMKOL) - TIMOFF
 140           IF ((TIME.GE.TIME1) .AND. (TIME.LE.TIME2)) THEN
C                                       Between entries
                  IF (TIME2.NE.TIME1) THEN
                     WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
                  ELSE
                     WT1 = 1.0
                     END IF
C                                       Before first time
               ELSE IF (TIME.LT.TIME1) THEN
                  WT1 = 1.0
C                                       After last time
               ELSE IF (IPNT2.GE.NUMTIM) THEN
                  WT1 = 0.0
C                                       Shift in interpolation arrays
               ELSE
                  IPNT1 = IPNT1 + 1
                  TIME1 = WRKTIM(IPNT1)
                  IPNT2 = IPNT2 + 1
                  TIME2 = WRKTIM(IPNT2)
                  GO TO 140
                  END IF
C
               WT2 = 1.0 - WT1
C                                       Delay
               DELCOR = RECR(DELKOL) + WT1 * WORK1(IPNT1) +
     *            WT2 * WORK1(IPNT2)
               RECR(DELKOL) = DELCOR
C                                       Dispersion
               DISP = RECR(DISKOL) + WT1 * WORK2(IPNT1) +
     *            WT2 * WORK2(IPNT2)
               RECR(DISKOL) = DISP
               END IF
C                                       Rewrite record
            CALL TABIO ('WRIT', IRCODE, LOOPR, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
            END IF
 200     CONTINUE
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFAN
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('MBDREF: TABIO ERROR',I3,' RE-REFERENCING ANT ',I3,' TO ',
     *   I3)
      END
