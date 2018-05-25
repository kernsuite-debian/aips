      SUBROUTINE SDCSET (TIME, IERR)
C-----------------------------------------------------------------------
C! Interpolates single dish calibration data for current time.
C# SDish Calibration
C-----------------------------------------------------------------------
C;  Copyright (C) 1995, 2000
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
C   Single dish calibration routine: Gets next set of calibration data
C   in CURCAL, does linear interpolation in time between time entries in
C   CALTAB.
C   Inputs:
C      TIME     R        Current time (of data) in days.
C   Inputs from common /SELCAL/:
C      CALTAB   R(*,2)   Cal. table from gain table file
C                        Values in order:
C                        By beam (NUMANT)
C                           By IF (NUMIF)
C                              By Polarization (NUMPOL)
C                                  Amplitude factor,
C                                  offset (before factor)
C                                  RA correction
C                                  Dec correction.
C      LCLTAB   I        Number of values in CALTAB per entry (4)
C      CALTIM   R(3)     Time of two cal. entries; third value is
C                        time of current values.
C      ICALP1   I        Pointer in CALTAB, CALTIM to previous time.
C      ICALP2   I        Pointer in CALTAB, CALTIM to next time.
C   Output:
C      IERR     I        Return error code, 0=>OK else error.
C   Output to common /SELCAL/:
C      LCALTM   R        Time of current calibration.  If gain file
C                        is exhausted then 1.0E20 is returned.
C      CURCAL   R(*)     Current calibration information.
C                        Values in order:
C                        By beam (NUMANT)
C                           By IF (EIF-BIF+1)
C                              By Polarization (NUMPOL)
C                                  Amplitude factor,
C                                  offset (before factor)
C                                  RA correction
C                                  Dec correction.
C-----------------------------------------------------------------------
      REAL      TIME
      INTEGER   IERR
C
      INTEGER   IANT, IIF, IPOL, INDEX, JNDEX, JCALP1, JCALP2, MAX
      LOGICAL   GOOD1, GOOD2
      REAL      WTT1, WTT2, GM1, GA1, GM2, GA2, RF1, RF2, DF1, DF2,
     *   DELTA, WT1, WT2
      INCLUDE 'INCS:PUVD.INC'
      INCLUDE 'INCS:DSEL.INC'
      INCLUDE 'INCS:DHDR.INC'
      INCLUDE 'INCS:DUVH.INC'
      INCLUDE 'INCS:DDCH.INC'
C-----------------------------------------------------------------------
      IERR = 0
C                                       Set pointer
      JCALP2 = MAX (1, ICALP2)
C                                       See if time for new table entry
      IF ((TIME.GT.CALTIM(JCALP2)) .OR. (ICALP2.LT.0))
     *   CALL SDCGET (TIME, IERR)
      IF (IERR.NE.0) GO TO 999
C                                       Reset pointers
      JCALP1 = MAX (1, ICALP1)
      JCALP2 = MAX (1, ICALP2)
C                                       See if calibration needs update;
C                                       every 0.2 of solution interval.
      DELTA = (TIME - LCALTM)
      IF (DELTA.LE.(0.2*(CALTIM(JCALP2)-CALTIM(JCALP1)))) GO TO 999
C                                       Try to aviod too much work.
      IF ((ICALP1.LT.-1) .OR. (ICALP2.LT.-1)) GO TO 999
      IF (ICALP1.LT.0) ICALP1 = -2
      IF (ICALP2.LT.0) ICALP2 = -2
      LCALTM = TIME
C                                       If all of gain table read set
C                                       to big value to stop further
C                                       calls.
      IF (ICALP2.LT.0) LCALTM = 1.0E20
      CALTIM(3) = TIME
C                                       Set interpolation weights.
      WTT1 = 0.0
      IF ((ICALP1.GT.0) .AND. (CALTIM(JCALP1).NE.CALTIM(JCALP2)))
     *   WTT1 = (CALTIM(JCALP2) - TIME) / (CALTIM(JCALP2) -
     *   CALTIM(JCALP1))
      WTT2 = 1.0 - WTT1
C                                       See if only gain before data
      IF (ICALP2.LT.0) WTT1 = 1.0
      IF (ICALP2.LT.0) WTT2 = 0.0
C                                       Initialize indices for
C                                       CURCAL and CALTAB
      INDEX = 1
      JNDEX = 1
C                                       Loop thru beams
      DO 500 IANT = 1,NUMANT
C                                       Loop thru IF
         DO 400 IIF = BIF,EIF
C                                       Loop thru polarization
            DO 300 IPOL = 1,NUMPOL
C                                       Initialize soln with blanks
               CURCAL(INDEX) = FBLANK
               CURCAL(INDEX+1) = FBLANK
               CURCAL(INDEX+2) = FBLANK
               CURCAL(INDEX+3) = FBLANK
C                                       Check for blanked soln.
               GOOD1 = (CALTAB(JNDEX,JCALP1).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+1,JCALP1).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+2,JCALP1).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+3,JCALP1).NE.FBLANK) .AND.
     *                 (WTT1.GT.0.0)
               GOOD2 = (CALTAB(JNDEX,JCALP2).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+1,JCALP2).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+2,JCALP2).NE.FBLANK) .AND.
     *                 (CALTAB(JNDEX+3,JCALP2).NE.FBLANK) .AND.
     *                 (WTT2.GT.0.0)
               IF (.NOT.(GOOD1.OR.GOOD2)) GO TO 290
 230           WT1 = WTT1
               WT2 = WTT2
               IF (.NOT.GOOD1) THEN
                  WT1 = 0.0
                  WT2 = 1.0
                  END IF
               IF (.NOT.GOOD2) THEN
                  WT1 = 1.0
                  WT2 = 0.0
                  END IF
C                                       Set values, initial gain.
               GM1 = 0.0
               GA1 = 0.0
               GM2 = 0.0
               GA2 = 0.0
               RF1 = 0.0
               DF1 = 0.0
               RF2 = 0.0
               DF2 = 0.0
               IF (GOOD1) THEN
                  GM1 = CALTAB(JNDEX,JCALP1)
                  GA1 = CALTAB(JNDEX+1,JCALP1)
                  RF1 = CALTAB(JNDEX+2,JCALP1)
                  DF1 = CALTAB(JNDEX+3,JCALP1)
                  END IF
               IF (GOOD2) THEN
                  GM2 = CALTAB(JNDEX,JCALP2)
                  GA2 = CALTAB(JNDEX+1,JCALP2)
                  RF2 = CALTAB(JNDEX+2,JCALP2)
                  DF2 = CALTAB(JNDEX+3,JCALP2)
                  END IF
C                                       Interpolate
 250           CURCAL(INDEX) = WT1 * GM1 + WT2 * GM2
               CURCAL(INDEX+1) = WT1 * GA1 + WT2 * GA2
               CURCAL(INDEX+2) = WT1 * RF1 + WT2 * RF2
               CURCAL(INDEX+3) = WT1 * DF1 + WT2 * DF2
C                                       Update indices
 290           INDEX = INDEX + LCUCAL
               JNDEX = JNDEX + LCLTAB
 300           CONTINUE
 400        CONTINUE
 500     CONTINUE
C
 999  RETURN
      END
