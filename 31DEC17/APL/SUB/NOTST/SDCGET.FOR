      SUBROUTINE SDCGET (TIME, IERR)
C-----------------------------------------------------------------------
C! Sets up to interpolate in Single dish calibration (CS) table.
C# SDish Calibration EXP-appl
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 1998
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
C   Single dish calibration routine
C    Sets up for interpolation in cal (CS) table, reads values from cal
C    table.  Assumes only valid, selected data in open cal table.
C    Uses calls to TABIO directly for efficiency.
C     Inputs:
C      TIME         R    Current data time.
C     Inputs from common /SELCAL/:
C      CLBUFF(*)    I    Cal table I/O TABIO buffer
C      ICLRNO       I    Current cal record number
C      NCLINR       I    Number of cal records in file.
C      NUMANT       I    Number of beams
C      NUMPOL       I    Number of IFs per group (polarizations)
C      NUMIF        I    Number of IFs.
C     Output:
C      IERR         I    Return error code 0=>OK, else failed.
C     Output to common /SELCAL/:
C      CALTAB(*,2)  R    Cal. table from cal table file
C                        Values in order:
C                        By beam (NUMANT)
C                           By IF (NUMIF)
C                              By Polarization (NUMPOL)
C                                  Amplitude factor,
C                                  offset (before factor)
C                                  RA correction
C                                  Dec correction.
C      LCLTAB       I   Number of values in CALTAB per entry (4)
C      CALTIM(3)    R    Time of two cal. entries; third value is
C                        time of current values.
C      ICALP1       I    Pointer in CALTAB, CALTIM to previous time.
C      ICALP2       I    Pointer in CALTAB, CALTIM to next time.
C-----------------------------------------------------------------------
      INTEGER   IERR, BEMNO, IFNO, LGREC,
     *   LGRECF, TIMKOL, RAKOL, DECKOL, BEMKOL, SUBKOL,
     *   F1KOL, O1KOL, RF1KOL, DF1KOL, F2KOL, O2KOL, RF2KOL, DF2KOL,
     *   RECI(1), IFOFF, I, INDEX, LIMIT, NBLANK
      LOGICAL   T, F, GOT1
      REAL      TIME
      DOUBLE PRECISION    RECD(1), CURTIM
      INCLUDE 'INCS:PUVD.INC'
      REAL     RECR(5+8*MAXIF)
      INCLUDE 'INCS:DDCH.INC'
      INCLUDE 'INCS:DMSG.INC'
      INCLUDE 'INCS:DSEL.INC'
      EQUIVALENCE (RECI, RECR, RECD)
      EQUIVALENCE (CLKOLS(1), TIMKOL),
     *   (CLKOLS(2),RAKOL), (CLKOLS(3), DECKOL),
     *   (CLKOLS(4),BEMKOL), (CLKOLS(5), SUBKOL),
     *   (CLKOLS(6), F1KOL), (CLKOLS(7),O1KOL),
     *   (CLKOLS(8), RF1KOL), (CLKOLS(9), DF1KOL),
     *   (CLKOLS(10), F2KOL), (CLKOLS(11),O2KOL),
     *   (CLKOLS(12), RF2KOL), (CLKOLS(13), DF2KOL)
      DATA T, F /.TRUE.,.FALSE./
C-----------------------------------------------------------------------
C                                       Check if first call
      IF (ICALP2.LE.0) THEN
C                                       Blank fill CALTAB
         NBLANK = NUMANT * (EIF-BIF+1) * NUMPOL * LCLTAB
         DO 40 I = 1,NBLANK
            CALTAB(I,1) = FBLANK
            CALTAB(I,2) = FBLANK
 40         CONTINUE
C                                       Read until selected time.
         LGREC = NUMPOL * LCLTAB
         LGRECF = LGREC * (EIF - BIF + 1)
         ICALP1 = 2
         ICALP2 = 1
         GOT1 = F
         LIMIT = ICLRNO
         DO 90 I = LIMIT,NCLINR
            ICLRNO = I
            CALL TABIO ('READ', 0, ICLRNO, RECR, CLBUFF, IERR)
            IF (IERR.LT.0) GO TO 90
            IF (IERR.NE.0) GO TO 990
C                                       Check SUBARRAY
            IF ((RECI(SUBKOL).NE.SUBARR) .AND. (RECI(SUBKOL).GT.0))
     *         GO TO 90
C                                       See if time past data time.
            IF (RECR(TIMKOL).GT.TIME) GO TO 100
C                                       Fill in values
            GOT1 = T
            CALTIM(1) = RECR(TIMKOL)
C                                       Loop over IF
            BEMNO = RECI(BEMKOL)
            DO 80 IFNO = BIF,EIF
               IFOFF = IFNO - 1
               INDEX = LGRECF * (BEMNO-1) +  LGREC * (IFNO-BIF) + 1
               CALTAB(INDEX,1) = RECR(F1KOL+IFOFF)
               CALTAB(INDEX+1,1) = RECR(O1KOL+IFOFF)
               CALTAB(INDEX+2,1) = RECR(DF1KOL+IFOFF)
               CALTAB(INDEX+3,1) = RECR(RF1KOL+IFOFF)
C                                       Second polarization
               IF (NUMPOL.GT.1) THEN
                  INDEX = INDEX + LCLTAB
                  CALTAB(INDEX,1) = RECR(F2KOL+IFOFF)
                  CALTAB(INDEX+1,1) = RECR(O2KOL+IFOFF)
                  CALTAB(INDEX+2,1) = RECR(RF2KOL+IFOFF)
                  CALTAB(INDEX+3,1) = RECR(DF2KOL+IFOFF)
                  END IF
 80            CONTINUE
 90         CONTINUE
C                                       After time, see if have before
C                                       Only have cal after data time.
 100     IF (.NOT.GOT1) THEN
            ICALP1 = -1
            ICALP2 = 1
            GO TO 310
            END IF
         END IF
C                                       Next record
C                                       Initialize CALTAB
 300  ICALP1 = ICALP2
      ICALP2 = 3 - ICALP2
C                                       Check if Cal. table exhausted,
C                                       If so just mark tables,
      IF (ICLRNO.GE.NCLINR) ICALP2 = -1
      IF (ICLRNO.GE.NCLINR) GO TO 999
C                                       Blank fill table
 310  NBLANK = NUMANT * (EIF-BIF+1) * NUMPOL * LCLTAB
      DO 340 I = 1,NBLANK
         CALTAB(I,ICALP2) = FBLANK
 340     CONTINUE
      LIMIT = ICLRNO
      DO 390 I = LIMIT,NCLINR
         ICLRNO = I
         CALL TABIO ('READ', 0, ICLRNO, RECR, CLBUFF, IERR)
         IF (IERR.LT.0) GO TO 390
         IF (IERR.NE.0) GO TO 990
C                                       Check SUBARRAY
         IF ((RECI(SUBKOL).NE.SUBARR) .AND. (RECI(SUBKOL).GT.0) .AND.
     *      (SUBARR.NE.0)) GO TO 390
         IF (I.EQ.LIMIT) CURTIM = RECR(TIMKOL)
C                                       See if time past data time.
         IF (RECR(TIMKOL).GT.CURTIM) GO TO 400
         CALTIM(ICALP2) = CURTIM
C                                       Fill in values
         GOT1 = T
C                                       Loop over IF
         BEMNO = RECI(BEMKOL)
         DO 380 IFNO = BIF,EIF
            IFOFF = IFNO - 1
            INDEX = LGRECF * (BEMNO-1) +  LGREC * (IFNO-BIF) + 1
            CALTAB(INDEX,ICALP2) = RECR(F1KOL+IFOFF)
            CALTAB(INDEX+1,ICALP2) = RECR(O1KOL+IFOFF)
            CALTAB(INDEX+2,ICALP2) = RECR(RF1KOL+IFOFF)
            CALTAB(INDEX+3,ICALP2) = RECR(DF1KOL+IFOFF)
            IF (NUMPOL.LE.1) GO TO 360
C                                       Second polarization
               INDEX = INDEX + LCLTAB
               CALTAB(INDEX,ICALP2) = RECR(F2KOL+IFOFF)
               CALTAB(INDEX+1,ICALP2) = RECR(O2KOL+IFOFF)
               CALTAB(INDEX+2,ICALP2) = RECR(RF2KOL+IFOFF)
               CALTAB(INDEX+3,ICALP2) = RECR(DF2KOL+IFOFF)
 360           CONTINUE
 380        CONTINUE
 390     CONTINUE
C                                       Finished file
      ICLRNO = NCLINR
      GO TO 999
C                                       Check if correct time
 400  IF (CURTIM.LT.TIME) GO TO 300
      GO TO 999
C                                       TABIO error
 990  WRITE (MSGTXT,1400) IERR
C                                       Error
      CALL MSGWRT (8)
C
 999  RETURN
C-----------------------------------------------------------------------
 1400 FORMAT ('SDCGET: ERROR ',I3,' READING CALIBRATION TABLE')
      END
