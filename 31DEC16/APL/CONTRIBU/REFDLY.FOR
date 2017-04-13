      SUBROUTINE REFDLY (ISUB, REFA, ANT, TIMKOL, SUBKOL, ANTKOL,
     *   SOUKOL, FRQKOL, REFKOL, DLYKOL, BUFFER, MXTIME, WRKTIM, WORK2,
     *   WORK3, IRET)
C-----------------------------------------------------------------------
C! Adjusts the reference antenna for delays in an SN table.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995
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
C-----------------------------------------------------------------------
C   Routine to rereference a delay like column in an open table.
C   A single IF and polarization is processed.   All valid entries are
C   included.
C      Note: this routine should be called before REFFAZ.  This routine
C   does not modify the reference antennas in the table
C   Inputs:
C      ISUB     I  Subarray number, 0=>1
C      REFA     I  Primary reference antenna
C      ANT      I  Secondary reference antenna
C      TIMKOL   I  Time column pointer.
C      SUBKOL   I  Subarray column pointer
C      ANTKOL   I  Antenna column pointer
C      SOUKOL   I  Source ID column pointer
C      FRQKOL   I  FQ id column pointer
C      REFKOL   I  Reference antenna pointer
C      DLYKOL   I  Delay  column pointer
C      MXTIME   I  Dimension of work arrays
C   Input/Output:
C      BUFFER   I(*) Table I/O buffer
C      WRKTIM   R(*) Work array
C      WORK2    R(*) Work array
C      WORK3    R(*) Work array
C   Output:
C      IRET     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   ISUB, REFA, ANT, TIMKOL, SUBKOL, ANTKOL, SOUKOL, FRQKOL,
     *   REFKOL, DLYKOL, BUFFER(*), MXTIME, IRET
      REAL      WRKTIM(MXTIME), WORK2(MXTIME), WORK3(MXTIME)

C
      INTEGER   LOOPR, IRCODE, NUMTIM, IPNT1, IPNT2, NUMREC, SUB, SA
      DOUBLE PRECISION TIMOFF, TIME, TIME1, TIME2
      REAL      WT1, WT2, SMOTIM, DLY1, DLY2
      INTEGER   RECORD(1024)
      REAL      RECR(1024)
      DOUBLE PRECISION RECD(512)
      EQUIVALENCE (RECORD, RECR, RECD)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
C                                       Loop thru table referring ANT
C                                       to REFA.
      SUB = MAX (1, ISUB)
      NUMTIM = 0
      NUMREC = BUFFER(5)
      DO 100 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) GO TO 100
         IF (IRET.NE.0) GO TO 900
         IF ((RECORD(ANTKOL).NE.ANT) .AND. (RECORD(ANTKOL).NE.REFA))
     *      GO TO 100
         IF ((RECORD(REFKOL).NE.ANT) .AND. (RECORD(REFKOL).NE.REFA))
     *      GO TO 100
         IF (RECORD(ANTKOL).EQ.RECORD(REFKOL)) GO TO 100
         IF (RECR(DLYKOL).EQ.FBLANK) GO TO 100
C                                       Right subarray?
         SA = RECORD(SUBKOL)
         IF ((SA.NE.SUB) .AND. (SA.GT.0)) GO TO 100
         IF (NUMTIM.GE.MXTIME) GO TO 100
            NUMTIM = NUMTIM + 1
            IF (NUMTIM.EQ.1) TIMOFF = RECD(TIMKOL)
            WRKTIM(NUMTIM) = RECD(TIMKOL) - TIMOFF
            IF (RECORD(REFKOL).NE.ANT) THEN
C                                       REFA is reference ant
               WORK3(NUMTIM) = RECR(DLYKOL)
            ELSE
C                                       ANT is reference ant
               WORK3(NUMTIM) = -RECR(DLYKOL)
               END IF
 100     CONTINUE
      IRET = 0
      IF (NUMTIM.LE.0) GO TO 999
C                                       Smooth (2 sec to extrapolate)
C                                       This doesn't really do anything
      SMOTIM = 2.0 / 86400.0
      CALL BOXSMO (SMOTIM, WRKTIM, WORK3, NUMTIM, WORK2)
C                                       Set up for interpolation
      IPNT1 = 1
      IPNT2 = 2
      TIME1 = WRKTIM(1)
      TIME2 = WRKTIM(2)
      IF (NUMTIM.EQ.1) THEN
         IPNT2 = 1
         TIME2 = TIME1
         END IF
C                                       Loop thru table changing any
C                                       data with ref=ANT to ref=REFA
      DO 200 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IRET)
         IF (IRET.LT.0) GO TO 200
         IF (IRET.NE.0) GO TO 900
C                                       Right subarray?
         SA = RECORD(SUBKOL)
         IF ((SA.NE.SUB) .AND. (SA.GT.0)) GO TO 200
         IF (RECORD(REFKOL).NE.ANT) GO TO 200
            IF (RECR(DLYKOL).EQ.FBLANK) GO TO 190
C                                       Interpolate
               TIME = RECD(TIMKOL) - TIMOFF
 140           IF ((TIME.GE.TIME1) .AND. (TIME.LE.TIME2)) THEN
C                                       Between entries
                  IF (TIME2.NE.TIME1) THEN
                     WT1 = 1.0 - ((TIME-TIME1) / (TIME2-TIME1))
                  ELSE
                     WT1 = 1.0
                     END IF
               ELSE IF (TIME.LT.TIME1) THEN
C                                       Before first time
                  WT1 = 1.0
               ELSE IF (IPNT2.GE.NUMTIM) THEN
C                                       After last time
                  WT1 = 0.0
               ELSE
C                                       Shift in interpolation arrays
                  IPNT1 = IPNT1 + 1
                  TIME1 = WRKTIM(IPNT1)
                  IPNT2 = IPNT2 + 1
                  TIME2 = WRKTIM(IPNT2)
                  GO TO 140
                  END IF
C                                       Interpolate
                  WT2 = 1.0 - WT1
                  DLY1 = WORK2(IPNT1)
                  DLY2 = WORK2(IPNT2)
                  RECR(DLYKOL) = RECR(DLYKOL) + WORK2(IPNT1) * WT1 +
     *               WORK2(IPNT2) * WT2
C                                       Rewrite record
 190        CALL TABIO ('WRIT', IRCODE, LOOPR, RECORD, BUFFER, IRET)
            IF (IRET.NE.0) GO TO 900
 200     CONTINUE
      IRET = 0
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IRET, ANT, REFA
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('REFDLY: TABIO ERROR',I3,' REREFERENCING ANT ',I3,' TO ',
     *   I3)
      END
