      SUBROUTINE REFCNT (ISUB, NUMIF, NUMANT, SUBKOL, WT1KOL, WT2KOL,
     *   RF1KOL, RF2KOL, BUFFER, ANTUSE, IERR)
C-----------------------------------------------------------------------
C! Counts reference antenna usage in an SN table.
C# UV Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2015
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
C   Routine to determine antenna usage as the reference antenna in an
C   open SN table.  All IFs are examined.
C   All valid entries are included.
C   Inputs:
C      ISUB     I  Subarray number, 0=>1
C      NUMIF    I  Number of IFs
C      NUMANT   I  Maximum antenna number.
C      SUBKOL   I  Subarray column pointer.
C      WT1KOL   I  Weight 1 column pointer.
C      WT2KOL   I  Weight 2 column pointer <1 => not present
C      RF1KOL   I  Reference antenna 1 pointer
C      RF2KOL   I  Reference antenna 2 pointer <1 => not present
C   Input/Output:
C      BUFFER   I(*) Table I/O buffer
C   Output:
C      ANTUSE   I(*) Count per antenna of uses as reference antenna.
C      IERR     I  Error code, 0=OK else failed.
C-----------------------------------------------------------------------
      INTEGER   ISUB, NUMIF, NUMANT, SUBKOL, WT1KOL, WT2KOL, RF1KOL,
     *   RF2KOL, BUFFER(*), ANTUSE(NUMANT), IERR
C
      INTEGER   LOOPR, IRCODE, IIF, IREF, NUMREC, SUB, SA
      INTEGER   RECORD(1024)
      REAL      RECR(1024)
      DOUBLE PRECISION RECD(512)
      EQUIVALENCE (RECORD, RECR, RECD)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      DATA IRCODE /0/
C-----------------------------------------------------------------------
      CALL FILL (NUMANT, 0, ANTUSE)
      NUMREC = BUFFER(5)
      SUB = MAX (1, ISUB)
      DO 20 LOOPR = 1,NUMREC
         CALL TABIO ('READ', IRCODE, LOOPR, RECORD, BUFFER, IERR)
         IF (IERR.LT.0) GO TO 20
         IF (IERR.NE.0) GO TO 900
C                                       Right subarray?
         SA = RECORD(SUBKOL)
         IF ((SA.NE.SUB) .AND. (SA.GT.0)) GO TO 20
C                                       Loop over IFs
            DO 10 IIF = 1,NUMIF
               IF (RECR(WT1KOL+IIF-1).GT.0.0) THEN
                  IREF = RECORD(RF1KOL+IIF-1)
                  IF ((IREF.GT.0) .AND. (IREF.LE.NUMANT))
     *               ANTUSE(IREF) = ANTUSE(IREF) + 1
                  END IF
               IF ((WT2KOL.GT.0) .AND. (RECR(WT2KOL+IIF-1).GT.0.0))
     *            THEN
                  IREF = RECORD(RF2KOL+IIF-1)
                  IF ((IREF.GT.0) .AND. (IREF.LE.NUMANT))
     *               ANTUSE(IREF) = ANTUSE(IREF) + 1
                  END IF
 10            CONTINUE
 20         CONTINUE
      IERR = 0
      GO TO 999
C                                       TABIO error
 900  WRITE (MSGTXT,1900) IERR
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1900 FORMAT ('REFCNT: TABIO ERROR',I3,' COUNTING REFERENCE ANTENNAS')
      END
